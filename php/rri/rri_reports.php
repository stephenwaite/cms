<?php

/**
 * The radiology reports of the RRI site, fetched from its FHIR API the way reads.php does:
 * the patient by MRN, then the visit's Observations by visit number. Needs reads.php's
 * environment (BASE_OEMR_URL, OEMR_RRI_SITE_ID, OEMR_TEST_CLIENT_ID, OEMR_RRI_USERNAME,
 * OEMR_RRI_PASSWORD).
 */

declare(strict_types=1);

require_once __DIR__ . '/measure_run.php';

use GuzzleHttp\Client;

/**
 * One login, kept until it runs out; patients and visits looked up once each.
 */
final class RriReports
{
    private Client $client;
    private string $base;
    private string $site;
    private ?string $token = null;
    /** @var array<string, ?string> patient uuid by account number */
    private array $patients = [];
    /** @var array<string, list<array{display: string, date: string, text: string}>> by account and visit */
    private array $visits = [];

    public function __construct()
    {
        $this->base = (string) getenv('BASE_OEMR_URL');
        $this->site = (string) getenv('OEMR_RRI_SITE_ID');
        if ($this->base === '' || $this->site === '') {
            measFail('BASE_OEMR_URL and OEMR_RRI_SITE_ID are not set (the environment reads.php runs in)');
        }
        $this->client = new Client(['verify' => false, 'http_errors' => false, 'timeout' => 60, 'connect_timeout' => 10]);
    }

    private function token(): string
    {
        if ($this->token === null) {
            $response = $this->client->post($this->base . '/oauth2/' . $this->site . '/token', [
                'form_params' => [
                    'grant_type' => 'password',
                    'client_id' => getenv('OEMR_TEST_CLIENT_ID'),
                    'scope' => 'openid fhirUser online_access user/Observation.read user/Patient.read',
                    'user_role' => 'users',
                    'username' => getenv('OEMR_RRI_USERNAME'),
                    'password' => getenv('OEMR_RRI_PASSWORD'),
                ],
            ]);
            $body = json_decode((string) $response->getBody(), true);
            if ($response->getStatusCode() !== 200 || !is_string($body['access_token'] ?? null)) {
                measFail('the RRI site refused the login (HTTP ' . $response->getStatusCode() . ')');
            }
            $this->token = $body['access_token'];
        }

        return $this->token;
    }

    /**
     * @return ?array<string, mixed> the decoded bundle; null when the request failed
     */
    private function get(string $path): ?array
    {
        for ($try = 0; $try < 2; $try++) {
            $response = $this->client->get($this->base . '/apis/' . $this->site . '/fhir/' . $path, [
                'headers' => ['Authorization' => 'Bearer ' . $this->token(), 'Accept' => 'application/json'],
            ]);
            if ($response->getStatusCode() === 401) {
                // The token ran out: log in once more.
                $this->token = null;
                continue;
            }
            $body = json_decode((string) $response->getBody(), true);

            return $response->getStatusCode() === 200 && is_array($body) ? $body : null;
        }

        return null;
    }

    /**
     * What the site holds for a visit, one entry per Observation: the procedure it names,
     * its date (yyyymmdd) and the report text. Empty when the patient or the visit is not
     * found.
     *
     * @return list<array{display: string, date: string, text: string}>
     */
    public function visit(string $acct, string $visit): array
    {
        $this->report($acct, $visit, '', '');
        $mrn = ltrim($acct, '0');
        $external = str_starts_with($visit, '0') ? '1' . $visit : $visit;

        return $this->visits[$mrn . '|' . $external] ?? [];
    }

    /**
     * The report for a charge: the visit's Observation that names the CPT, dated the date of
     * service. '' when there is none.
     *
     * @param string $visit the charge's visit number
     * @param string $dos yyyymmdd
     */
    public function report(string $acct, string $visit, string $dos, string $cpt): string
    {
        $mrn = ltrim($acct, '0');
        if (!array_key_exists($mrn, $this->patients)) {
            $bundle = $this->get('Patient?identifier=' . rawurlencode($mrn));
            $id = $bundle['entry'][0]['resource']['id'] ?? null;
            $this->patients[$mrn] = is_string($id) ? $id : null;
        }
        $uuid = $this->patients[$mrn];
        if ($uuid === null) {
            return '';
        }
        // As reads.php: a visit number that starts with 0 is stored with a 1 before it.
        $external = str_starts_with($visit, '0') ? '1' . $visit : $visit;
        $key = $mrn . '|' . $external;
        if (!isset($this->visits[$key])) {
            $this->visits[$key] = [];
            $bundle = $this->get('Observation?patient=' . rawurlencode($uuid) . '&external_id=' . rawurlencode($external));
            foreach ($bundle['entry'] ?? [] as $entry) {
                $r = $entry['resource'] ?? [];
                $this->visits[$key][] = [
                    'display' => (string) ($r['code']['coding'][0]['display'] ?? ''),
                    // The date as the timestamp gives it, in its own offset: what reads.php compares.
                    'date' => substr(str_replace('-', '', (string) ($r['effectiveDateTime'] ?? '')), 0, 8),
                    'text' => (string) ($r['note'][0]['text'] ?? ''),
                ];
            }
        }
        foreach ($this->visits[$key] as $observation) {
            if ($observation['date'] === $dos && str_contains($observation['display'], $cpt) && trim($observation['text']) !== '') {
                return $observation['text'];
            }
        }

        return '';
    }
}

