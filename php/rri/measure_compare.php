<?php

/**
 * Answer the radiology quality measures (QMM26, 364, 405, 406) from the reports in Gifford's
 * charge file, and count what came back.
 *
 * Runs where the live charge file is (it reads patient data); only the summary it writes is free
 * of identifiers.
 *
 *   php measure_compare.php run FILE [--limit N] [--model M] [--effort E] [--workers 4] [--dry-run]
 *   php measure_compare.php run FILE [--limit N] --resume gifford-measure-run-<time>.json
 *   php measure_compare.php summary gifford-measure-run-<time>.json
 *
 * run reads the charge file and asks once per accession that carries a measure (by its CPT
 * codes, the patient's age and whether Medicare is primary: measure_suggest.php): the report,
 * the CPT codes, the age, the visit's diagnosis codes and the facility's clinical indication;
 * no names, numbers or dates. Each answer is saved as it arrives, so a run that stops can be
 * finished with --resume (same file and --limit). The first request is sent alone: a refused
 * key, model or setting stops the run there. It writes, in the current directory:
 *
 *   gifford-measure-run-<time>.json           the accessions asked (PATIENT DATA)
 *   gifford-measure-run-<time>.results.jsonl  each answer as it came (PATIENT DATA)
 *   gifford-measure-run-<time>.detail.csv     one line per accession and measure (PATIENT DATA)
 *   gifford-measure-run-<time>.summary.txt    counts only: the file to copy out
 *
 * summary writes the detail and summary files again from the saved answers, asking nothing.
 * Needs ANTHROPIC_API_KEY in the environment.
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
 * A file only its owner can read: the run, results and detail files hold patient data.
 *
 * @return resource
 */
function measOpenPrivate(string $path, string $mode)
{
    if (!file_exists($path)) {
        touch($path);
    }
    chmod($path, 0600);
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
 * The accessions of the file that carry a measure, each with its measures and the patient's
 * age on the date of service.
 *
 * @return array{list<array<string, mixed>>, int} the accessions, and how many carry a measure but have no report
 */
function measAccessions(string $file): array
{
    $found = [];
    $noReport = 0;
    foreach (dxReadAccessions($file) as $a) {
        $age = measAge((string) $a['dob'], (string) $a['dos']);
        $measures = measFor($a['cpts'], $age, dxPayerClass((string) $a['payer']) === 'medicare');
        if ($measures === []) {
            continue;
        }
        if (trim($a['report']) === '') {
            $noReport++;
            continue;
        }
        $found[] = $a + ['age' => $age, 'measures' => $measures];
    }

    return [$found, $noReport];
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
 * The detail (patient data) and summary (counts only) files, from the saved answers.
 */
function measWriteOutputs(string $runPath): void
{
    $meta = json_decode((string) file_get_contents($runPath), true);
    if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
        measFail("$runPath is not a run file");
    }
    $base = preg_replace('/\.json$/', '', $runPath);
    $results = measLoadResults($base . '.results.jsonl');

    $outcomes = [];
    $usage = [];
    $rows = [];       // one per accession and measure
    $studies = ['all' => 0, 'none' => 0, 'some' => 0];
    foreach ($meta['accessions'] as $id => $a) {
        $r = $results[(string) $id] ?? ['outcome' => 'not asked'];
        $outcomes[] = (string) $r['outcome'];
        foreach ($r['usage'] ?? [] as $name => $n) {
            $usage[$name] = ($usage[$name] ?? 0) + (int) $n;
        }
        if ($r['outcome'] !== 'succeeded') {
            continue;
        }
        $standing = 0;
        foreach ($a['measures'] as $measure) {
            $answer = $r['answers'][(string) $measure];
            [$final, $stands, $note] = measDecide((string) $measure, $answer, $a['gifford']);
            $standing += $stands ? 1 : 0;
            $rows[] = [
                'a' => $a, 'measure' => (string) $measure, 'given' => $answer['answer'], 'answer' => $final, 'stands' => $stands,
                'note' => $note, 'confidence' => $answer['confidence'], 'evidence' => $answer['evidence'], 'rationale' => $answer['rationale'],
            ];
        }
        $studies[$standing === count($a['measures']) ? 'all' : ($standing === 0 ? 'none' : 'some')]++;
    }

    $detail = measOpenPrivate($base . '.detail.csv', 'w');
    fputcsv($detail, ['accession', 'file_lines', 'cpt', 'location', 'payer', 'age', 'gifford_dx', 'measure', 'answer', 'code',
        'model_answer', 'confidence', 'coder_step', 'evidence', 'rationale'], ',', '"', '');
    foreach ($rows as $row) {
        $a = $row['a'];
        fputcsv($detail, [
            $a['accession'], implode(' ', $a['lines']), implode(' ', $a['cpts']), $a['location'], $a['payer'], $a['age'] ?? '',
            implode(' ', $a['gifford']), $row['measure'], $row['answer'], MEAS_OPTIONS[$row['measure']][$row['answer']][1],
            $row['given'], $row['confidence'], $row['stands'] ? 'stands' . ($row['note'] !== '' ? ': ' . $row['note'] : '') : $row['note'],
            $row['evidence'], $row['rationale'],
        ], ',', '"', '');
    }
    fclose($detail);

    $asked = count($meta['accessions']);
    $ok = count(array_filter($outcomes, static fn (string $o): bool => $o === 'succeeded'));
    $pct = static fn (int $n, int $of): string => $of > 0 ? sprintf('%d (%.1f%%)', $n, 100 * $n / $of) : (string) $n;
    $out = [
        sprintf('Gifford quality measures, %s; model %s, effort %s', $meta['created'], $meta['model'], $meta['effort']),
        sprintf('%d accessions sent; results: %s', $asked, measCounts($outcomes)),
        '',
        sprintf('Studies answered: %d', $ok),
        '  every measure stands without a coder   ' . $pct($studies['all'], $ok),
        '  some stand, some need a coder          ' . $pct($studies['some'], $ok),
        '  none stands                            ' . $pct($studies['none'], $ok),
        '  by location (answered): ' . measCounts(array_map(
            static fn (string $id): string => (string) $meta['accessions'][$id]['location'],
            array_values(array_filter(array_map('strval', array_keys($meta['accessions'])), static fn (string $id): bool => ($results[$id]['outcome'] ?? '') === 'succeeded'))
        )),
        '',
    ];
    foreach (array_keys(MEAS_OPTIONS) as $measure) {
        $measure = (string) $measure;
        $mine = array_values(array_filter($rows, static fn (array $r): bool => $r['measure'] === $measure));
        if ($mine === []) {
            continue;
        }
        $stand = array_values(array_filter($mine, static fn (array $r): bool => $r['stands']));
        $out[] = sprintf('Measure %s: %d questions; stand without a coder %s, coder confirms %d', $measure, count($mine), $pct(count($stand), count($mine)), count($mine) - count($stand));
        $out[] = '  confidence: ' . measCounts(array_column($mine, 'confidence'));
        foreach (MEAS_OPTIONS[$measure] as $n => [$text, $code]) {
            $with = array_values(array_filter($mine, static fn (array $r): bool => $r['answer'] === $n));
            if ($with === []) {
                continue;
            }
            $out[] = sprintf(
                '  %d %-9s %3d  (stand %d, coder %d)  %s',
                $n,
                $code === '' ? '-' : str_replace(' ', '+', $code),
                count($with),
                count(array_filter($with, static fn (array $r): bool => $r['stands'])),
                count(array_filter($with, static fn (array $r): bool => !$r['stands'])),
                $text
            );
        }
        $byDx = count(array_filter($mine, static fn (array $r): bool => $r['note'] === 'excluded by diagnosis'));
        if ($byDx > 0) {
            $out[] = sprintf(
                '  of the 4s, %d are by a cancer or screening diagnosis on the visit; the answer from the report was: %s',
                $byDx,
                measCounts(array_map(static fn (array $r): string => (string) $r['given'], array_values(array_filter($mine, static fn (array $r): bool => $r['note'] === 'excluded by diagnosis'))))
            );
        }
        $empty = count(array_filter($mine, static fn (array $r): bool => trim($r['evidence']) === ''));
        $out[] = sprintf('  answers with no words quoted from the report: %d', $empty);
        $out[] = '';
    }
    $out[] = 'Tokens: ' . ($usage === [] ? '-' : implode(', ', array_map(static fn (string $k, int $n): string => $k . ' ' . number_format($n), array_keys($usage), $usage)));
    $summary = implode("\n", $out) . "\n";
    file_put_contents($base . '.summary.txt', $summary);
    echo $summary, "\n";
    fwrite(STDERR, "wrote $base.detail.csv (patient data, keep it here) and $base.summary.txt (counts only)\n");
}

/**
 * @param array<string, string> $options
 */
function measRun(string $file, array $options): void
{
    $resume = $options['resume'] ?? null;
    $meta = null;
    if ($resume !== null) {
        $meta = json_decode((string) file_get_contents($resume), true);
        if (!is_array($meta) || !is_array($meta['accessions'] ?? null)) {
            measFail("$resume is not a run file");
        }
    }
    [$usable, $noReport] = measAccessions($file);
    if (isset($options['limit'])) {
        $usable = array_slice($usable, 0, (int) $options['limit']);
    }
    $ids = array_map(static fn (int $i): string => sprintf('m%05d', $i), array_keys($usable));
    fwrite(STDERR, sprintf(
        "%d accessions carry a measure (%s); %d more left out for having no report\n",
        count($usable),
        measCounts(array_merge(...array_column($usable, 'measures'), ...[[]])),
        $noReport
    ));
    $model = $meta['model'] ?? $options['model'] ?? 'claude-opus-5-5';
    $effort = $meta['effort'] ?? $options['effort'] ?? 'high';
    if (!in_array($effort, ['low', 'medium', 'high', 'xhigh', 'max'], true)) {
        measFail('--effort is low, medium, high, xhigh or max');
    }
    $requests = [];
    foreach ($usable as $i => $a) {
        $requests[$ids[$i]] = measRequest($model, $effort, $a['measures'], $a['cpts'], $a['age'], $a['gifford'], (string) $a['clinical'], $a['report']);
    }
    if (isset($options['dry-run'])) {
        $chars = array_sum(array_map(static fn (array $r): int => mb_strlen($r['messages'][0]['content']), $requests));
        fwrite(STDERR, sprintf(
            "dry run, nothing sent: %d requests, %s characters of reports, system prompt %s characters\n",
            count($requests),
            number_format($chars),
            number_format(mb_strlen(measSystemPrompt()))
        ));

        return;
    }

    $kept = ['accession', 'lines', 'cpts', 'gifford', 'location', 'pos', 'status', 'payer', 'age', 'measures'];
    if ($meta !== null) {
        $runPath = $resume;
        $then = array_map(static fn (array $a): string => (string) $a['accession'], $meta['accessions']);
        if (array_values($then) !== array_column($usable, 'accession') || array_map('strval', array_keys($then)) !== $ids) {
            measFail('this file with this selection does not give the accessions the run file was made from');
        }
    } else {
        $created = date('Ymd-His');
        $runPath = "gifford-measure-run-$created.json";
        if (file_exists($runPath)) {
            measFail("$runPath exists already: wait a second and run again");
        }
        $meta = ['created' => $created, 'model' => $model, 'effort' => $effort, 'file' => basename($file), 'accessions' => []];
        foreach ($usable as $i => $a) {
            $meta['accessions'][$ids[$i]] = array_intersect_key($a, array_flip($kept));
        }
        $handle = measOpenPrivate($runPath, 'w');
        fwrite($handle, json_encode($meta, JSON_THROW_ON_ERROR));
        fclose($handle);
    }
    $resultsPath = preg_replace('/\.json$/', '', $runPath) . '.results.jsonl';
    $done = measLoadResults($resultsPath);
    $todo = array_values(array_filter($ids, static fn (string $id): bool => ($done[$id]['outcome'] ?? '') !== 'succeeded'));
    fwrite(STDERR, sprintf("%d to ask, %d already answered; run file %s\n", count($todo), count($ids) - count($todo), $runPath));

    $apiKey = (string) getenv('ANTHROPIC_API_KEY');
    if ($apiKey === '' && $todo !== []) {
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
    $measures = [];
    foreach ($usable as $i => $a) {
        $measures[$ids[$i]] = $a['measures'];
    }

    if ($todo !== []) {
        // The first request alone: a refused key, model or setting stops here.
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
            'concurrency' => max(1, (int) ($options['workers'] ?? 4)),
            'fulfilled' => static function (array $result, string $id) use ($record, &$asked, $todo): void {
                $record($id, $result);
                if (++$asked % 25 === 0) {
                    fwrite(STDERR, "$asked of " . count($todo) . " asked\n");
                }
            },
        ]);
        $pool->promise()->wait();
    }
    fclose($results);
    measWriteOutputs($runPath);
    fwrite(STDERR, "if some errored, ask only those again with: --resume $runPath\n");
}

$command = $argv[1] ?? '';
if ($command === 'run') {
    [$positional, $options] = measArgs(array_slice($argv, 2), ['dry-run']);
    if (count($positional) !== 1) {
        measFail('run takes the charge file; see the top of this script');
    }
    measRun($positional[0], $options);
} elseif ($command === 'summary') {
    [$positional, $options] = measArgs(array_slice($argv, 2), []);
    if (count($positional) !== 1) {
        measFail('summary takes the run file; see the top of this script');
    }
    measWriteOutputs($positional[0]);
} else {
    measFail('usage: php measure_compare.php run FILE [options] | summary RUN.json; see the top of this script');
}
