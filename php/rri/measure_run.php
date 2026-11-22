<?php

/**
 * What the scripts that ask the quality measure questions share: arguments, the private
 * files, sending the requests with retries and saving each answer as it arrives.
 * measure_compare.php (Gifford's charge file) and measure_compare_rri.php (charcur) use it.
 */

declare(strict_types=1);

require_once __DIR__ . '/../../vendor/autoload.php';
require_once __DIR__ . '/measure_suggest.php';

use GuzzleHttp\Client;
use GuzzleHttp\Exception\ConnectException;
use GuzzleHttp\Pool;
use GuzzleHttp\Promise\PromiseInterface;
use Psr\Http\Message\ResponseInterface;

const MEAS_MAX_RETRIES = 5;
const MEAS_RETRY_STATUS = [429, 500, 502, 503, 504, 529];
const MEAS_FATAL_STATUS = [400, 401, 403, 404];

/**
 * Says why on stderr and stops. (No `never` return type: this also has to run on PHP 8.0.)
 */
function measFail(string $message): void
{
    fwrite(STDERR, $message . "\n");
    exit(1);
}

/**
 * @param list<string> $argv after the command
 * @param list<string> $flags options that take no value
 * @return array{list<string>, array<string, string>} positional arguments and --options
 */
function measArgs(array $argv, array $flags): array
{
    $positional = [];
    $options = [];
    for ($i = 0; $i < count($argv); $i++) {
        $arg = $argv[$i];
        if (!str_starts_with($arg, '--')) {
            $positional[] = $arg;
            continue;
        }
        $name = substr($arg, 2);
        if (in_array($name, $flags, true)) {
            $options[$name] = '1';
            continue;
        }
        if (!isset($argv[$i + 1]) || str_starts_with($argv[$i + 1], '--')) {
            measFail("--$name needs a value");
        }
        $options[$name] = $argv[++$i];
    }

    return [$positional, $options];
}

/**
 * A file only its owner and group can read (0660): the run, results, detail and saved
 * suggestion files hold patient data. Group read/write is the coding group's (rri) default,
 * so whichever coder runs the load can read and add to measure-suggested.jsonl. The mode is
 * set only on a file this user creates or owns; chmod on someone else's file would fail.
 *
 * @return resource
 */
function measOpenPrivate(string $path, string $mode)
{
    $created = !file_exists($path);
    if ($created) {
        touch($path);
    }
    if ($created || (function_exists('posix_geteuid') && fileowner($path) === posix_geteuid())) {
        chmod($path, 0660);
    }
    $handle = fopen($path, $mode);
    if ($handle === false) {
        measFail("cannot write $path");
    }

    return $handle;
}

/**
 * The last result recorded for each request id.
 *
 * @return array<string, array<string, mixed>>
 */
function measLoadResults(string $path): array
{
    $results = [];
    foreach (is_file($path) ? (file($path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) ?: []) : [] as $line) {
        $r = json_decode($line, true);
        if (is_array($r) && isset($r['id'])) {
            $results[(string) $r['id']] = $r;
        }
    }

    return $results;
}

/**
 * One accession's request, retried on rate limits, overloads and failed connections. Resolves to
 * the plain record kept for it; it never rejects.
 *
 * @param array<string, mixed> $request
 * @param list<string> $measures
 */
function measAsk(Client $client, string $apiKey, array $request, array $measures, int $attempt = 0, int $delayMs = 0): PromiseInterface
{
    $retry = static fn (int $seconds): PromiseInterface => measAsk($client, $apiKey, $request, $measures, $attempt + 1, $seconds * 1000);

    return $client->postAsync('/v1/messages', [
        'headers' => ['x-api-key' => $apiKey, 'anthropic-version' => DX_API_VERSION, 'content-type' => 'application/json'],
        'json' => $request,
        'delay' => $delayMs,
    ])->then(
        static function (ResponseInterface $response) use ($retry, $attempt, $measures) {
            $status = $response->getStatusCode();
            $body = json_decode((string) $response->getBody(), true);
            if ($status === 200) {
                return is_array($body) ? measParseResponse($body, $measures) : ['outcome' => 'errored', 'note' => 'the response was not JSON'];
            }
            if (in_array($status, MEAS_RETRY_STATUS, true)) {
                if ($attempt < MEAS_MAX_RETRIES) {
                    $after = (int) $response->getHeaderLine('retry-after');

                    return $retry($after > 0 ? min($after, 120) : 2 ** ($attempt + 1));
                }

                return ['outcome' => 'errored', 'note' => $status === 429 ? 'rate limited after retries' : "HTTP $status after retries"];
            }

            return [
                'outcome' => 'errored', 'note' => "HTTP $status", 'fatal' => in_array($status, MEAS_FATAL_STATUS, true),
                'message' => substr(is_array($body) ? (string) ($body['error']['message'] ?? '') : '', 0, 300),
            ];
        },
        static function (\Throwable $e) use ($retry, $attempt) {
            if ($e instanceof ConnectException && $attempt < MEAS_MAX_RETRIES) {
                return $retry(2 ** ($attempt + 1));
            }

            return ['outcome' => 'errored', 'note' => $e instanceof ConnectException ? 'connection failed after retries' : 'request failed'];
        }
    );
}

/**
 * "3 x12, 0 x9": each value with its count, most common first.
 *
 * @param list<string> $values
 */
function measCounts(array $values): string
{
    $counts = array_count_values($values);
    arsort($counts);

    return $counts === [] ? '-' : implode(', ', array_map(static fn ($v, int $n): string => "$v x$n", array_keys($counts), $counts));
}

/**
 * Ask every request that has no saved answer yet, appending each answer to the results file as
 * it arrives. The first one is sent alone: a refused key, model or setting stops the run there.
 *
 * @param array<string, array<string, mixed>> $requests by id
 * @param array<string, list<string>> $measures by id
 * @param list<string> $ids in the order to ask
 */
function measSendAll(array $requests, array $measures, array $ids, string $resultsPath, int $workers): void
{
    $done = measLoadResults($resultsPath);
    $todo = array_values(array_filter($ids, static fn (string $id): bool => ($done[$id]['outcome'] ?? '') !== 'succeeded'));
    fwrite(STDERR, sprintf("%d to ask, %d already answered\n", count($todo), count($ids) - count($todo)));
    if ($todo === []) {
        return;
    }
    $apiKey = (string) getenv('ANTHROPIC_API_KEY');
    if ($apiKey === '') {
        measFail('ANTHROPIC_API_KEY is not set');
    }
    $client = new Client([
        'base_uri' => getenv('ANTHROPIC_BASE_URL') ?: 'https://api.anthropic.com',
        'http_errors' => false, 'timeout' => 600, 'connect_timeout' => 10,
    ]);
    $results = measOpenPrivate($resultsPath, 'a');
    $record = static function (string $id, array $result) use ($results): void {
        fwrite($results, json_encode($result + ['id' => $id], JSON_THROW_ON_ERROR) . "\n");
        fflush($results);
    };
    $first = measAsk($client, $apiKey, $requests[$todo[0]], $measures[$todo[0]])->wait();
    $record($todo[0], $first);
    if ($first['fatal'] ?? false) {
        fclose($results);
        measFail("stopped at the first request: {$first['note']}: " . ($first['message'] ?? ''));
    }
    $rest = array_slice($todo, 1);
    $asked = 1;
    $pool = new Pool($client, (static function () use ($client, $apiKey, $requests, $measures, $rest) {
        foreach ($rest as $id) {
            yield $id => static fn (): PromiseInterface => measAsk($client, $apiKey, $requests[$id], $measures[$id]);
        }
    })(), [
        'concurrency' => $workers,
        'fulfilled' => static function (array $result, string $id) use ($record, &$asked, $todo): void {
            $record($id, $result);
            if (++$asked % 25 === 0) {
                fwrite(STDERR, "$asked of " . count($todo) . " asked\n");
            }
        },
    ]);
    $pool->promise()->wait();
    fclose($results);
}
