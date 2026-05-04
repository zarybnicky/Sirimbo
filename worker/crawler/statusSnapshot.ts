import type {
  IGetCrawlerFrontierStatusResult,
  IGetCrawlerScheduleStatusResult,
  IGetCrawlerWorkerJobStatusResult,
  IGetFrontierFailureGroupsResult,
} from './crawler.queries.ts';
import { loaderFor } from './handlers.ts';

const DEFAULT_HOST_SPACING_MS = 50;

type StatusFilter = {
  federation?: string;
  kind?: string;
};

type StatusInputs = {
  generatedAt: Date;
  filter: StatusFilter;
  frontierRows: IGetCrawlerFrontierStatusResult[];
  failureGroups: IGetFrontierFailureGroupsResult[];
  workerJobs: IGetCrawlerWorkerJobStatusResult[];
  scheduleRows: IGetCrawlerScheduleStatusResult[];
};

type StatusSeverity = 'error' | 'warning' | 'info';

export type StatusProblem = {
  severity: StatusSeverity;
  code:
    | 'unknown_loader'
    | 'loader_request_error'
    | 'fetch_failure'
    | 'process_failure'
    | 'worker_failure'
    | 'stale_backlog'
    | 'scheduler_gap'
    | 'process_gap'
    | 'queue_pressure';
  kind?: string;
  task?: string | null;
  host?: string | null;
  count: number;
  message: string;
  sample_id?: string | null;
  sample_key?: string | null;
  at?: Date | null;
};

type ScoredStatusProblem = StatusProblem & {
  score: number;
};

export type CrawlerStatusRow = IGetCrawlerFrontierStatusResult & {
  target: string;
  host: string | null;
  has_loader: boolean;
  is_stable: boolean;
  loader_request_error?: string;
  revalidate_period: string | null;
  revalidate_period_ms: number | null;
  deadline_at: Date | null;
  eta_at: Date | null;
  is_stale: boolean;
};

export type StatusSnapshot = {
  generated_at: string;
  filter: StatusFilter;
  problems: StatusProblem[];
  frontiers: CrawlerStatusRow[];
  failures: IGetFrontierFailureGroupsResult[];
  worker_jobs: IGetCrawlerWorkerJobStatusResult[];
  schedule: IGetCrawlerScheduleStatusResult[];
};

function kindId(federation: string, kind: string) {
  return `${federation}:${kind}`;
}

function dateMax(...values: Array<Date | null | undefined>) {
  const timestamps = values
    .filter((value): value is Date => value != null)
    .map((value) => value.getTime());
  return timestamps.length === 0 ? null : new Date(Math.max(...timestamps));
}

function severityRank(severity: StatusSeverity) {
  return severity === 'error' ? 3 : severity === 'warning' ? 2 : 1;
}

function sortProblems(a: ScoredStatusProblem, b: ScoredStatusProblem) {
  return (
    severityRank(b.severity) - severityRank(a.severity) ||
    b.score - a.score ||
    (a.at?.getTime() ?? Number.MAX_SAFE_INTEGER) -
      (b.at?.getTime() ?? Number.MAX_SAFE_INTEGER) ||
    String(a.kind ?? a.task ?? '').localeCompare(String(b.kind ?? b.task ?? ''))
  );
}

export function sortBacklogRows(a: CrawlerStatusRow, b: CrawlerStatusRow) {
  return (
    Number(b.is_stale) - Number(a.is_stale) ||
    b.unscheduled_fetch - a.unscheduled_fetch ||
    b.fetch_due - a.fetch_due ||
    (a.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) -
      (b.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) ||
    kindId(a.federation, a.kind).localeCompare(kindId(b.federation, b.kind))
  );
}

function frontierPriority(row: CrawlerStatusRow) {
  return (
    (row.has_loader ? 0 : 1_000_000) +
    row.process_error * 100_000 +
    row.fetch_error * 10_000 +
    row.fetch_transient * 1_000 +
    row.process_ready * 100 +
    row.fetch_due * 10 +
    row.fetch_pending
  );
}

function sortFrontiers(a: CrawlerStatusRow, b: CrawlerStatusRow) {
  return frontierPriority(b) - frontierPriority(a) || a.target.localeCompare(b.target);
}

function parseIntervalMs(value: string | null | undefined) {
  if (!value) return null;

  const units: Record<string, number> = {
    millisecond: 1,
    milliseconds: 1,
    ms: 1,
    second: 1_000,
    seconds: 1_000,
    sec: 1_000,
    secs: 1_000,
    s: 1_000,
    minute: 60_000,
    minutes: 60_000,
    min: 60_000,
    mins: 60_000,
    m: 60_000,
    hour: 3_600_000,
    hours: 3_600_000,
    hr: 3_600_000,
    hrs: 3_600_000,
    h: 3_600_000,
    day: 86_400_000,
    days: 86_400_000,
    d: 86_400_000,
    week: 604_800_000,
    weeks: 604_800_000,
    w: 604_800_000,
  };

  let total = 0;
  const matches = value.matchAll(/(\d+(?:\.\d+)?)\s*([a-zA-Z]+)/g);
  for (const match of matches) {
    const unit = units[match[2].toLowerCase()];
    if (!unit) return null;
    total += Number(match[1]) * unit;
  }

  return total > 0 ? total : null;
}

function enrichFrontierRows(rows: IGetCrawlerFrontierStatusResult[]): CrawlerStatusRow[] {
  return rows.map((row) => {
    const target = kindId(row.federation, row.kind);
    const loader = loaderFor(row.federation, row.kind);
    const revalidatePeriodMs = parseIntervalMs(loader?.revalidatePeriod);
    let host: string | null = null;
    let loaderRequestError: string | undefined;
    if (loader && row.sample_due_key != null) {
      try {
        host = loader.buildRequest(row.sample_due_key).url.host;
      } catch (e) {
        loaderRequestError = e instanceof Error ? e.message : String(e);
      }
    }

    const deadline_at =
      row.oldest_due_at && revalidatePeriodMs
        ? new Date(row.oldest_due_at.getTime() + revalidatePeriodMs)
        : null;
    const hasLoader = Boolean(loader);
    const fetchTransient = row.fetch_transient;
    const fetchError = row.fetch_error;
    const processError = row.process_error;

    return {
      ...row,
      target,
      host,
      has_loader: hasLoader,
      is_stable:
        hasLoader &&
        loaderRequestError == null &&
        row.total === row.done &&
        row.fetch_due === 0 &&
        row.fetch_pending === 0 &&
        fetchTransient === 0 &&
        fetchError === 0 &&
        row.process_ready === 0 &&
        processError === 0,
      loader_request_error: loaderRequestError,
      revalidate_period: loader?.revalidatePeriod ?? null,
      revalidate_period_ms: revalidatePeriodMs,
      deadline_at,
      eta_at: null,
      is_stale: false,
    };
  });
}

function computeQueueEta(
  rows: CrawlerStatusRow[],
  schedules: Map<string, IGetCrawlerScheduleStatusResult>,
  now: Date,
) {
  const byHost = new Map<string, CrawlerStatusRow[]>();
  for (const row of rows) {
    if (!row.host || row.fetch_due <= 0) continue;
    const hostRows = byHost.get(row.host);
    if (hostRows) hostRows.push(row);
    else byHost.set(row.host, [row]);
  }

  for (const [host, hostRows] of byHost.entries()) {
    const schedule = schedules.get(host);
    let tail =
      dateMax(
        now,
        schedule?.next_available_at,
        schedule?.next_run_at,
        schedule?.queue_tail_at,
      ) ?? now;

    hostRows.sort(
      (a, b) =>
        (a.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) -
          (b.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) ||
        kindId(a.federation, a.kind).localeCompare(kindId(b.federation, b.kind)),
    );

    for (const row of hostRows) {
      const spacing = schedule?.spacing ?? DEFAULT_HOST_SPACING_MS;
      row.eta_at = new Date(tail.getTime() + row.unscheduled_fetch * spacing);
      row.is_stale = Boolean(row.deadline_at && row.eta_at > row.deadline_at);
      tail = row.eta_at;
    }
  }
}

function loaderProblems(rows: CrawlerStatusRow[]): ScoredStatusProblem[] {
  const problems: ScoredStatusProblem[] = [];
  for (const row of rows) {
    if (!row.has_loader) {
      problems.push({
        severity: 'error',
        code: 'unknown_loader',
        kind: row.target,
        count: 1,
        message: `${row.target} has frontier rows but no loader`,
        score: 1_000_000,
      });
      continue;
    }
    if (!row.loader_request_error) continue;

    problems.push({
      severity: 'error',
      code: 'loader_request_error',
      kind: row.target,
      count: 1,
      message: `${row.target} loader request failed while deriving status metadata: ${row.loader_request_error}`,
      score: 900_000,
    });
  }
  return problems;
}

function frontierProblems(rows: CrawlerStatusRow[]): ScoredStatusProblem[] {
  const problems: ScoredStatusProblem[] = [];
  for (const row of rows) {
    const fetchError = row.fetch_error;
    const fetchTransient = row.fetch_transient;
    const processError = row.process_error;

    if (processError > 0) {
      problems.push({
        severity: 'error',
        code: 'process_failure',
        kind: row.target,
        count: processError,
        message: `${row.target} has ${processError} processing failures`,
        score: 800_000 + processError,
      });
    }
    if (fetchError > 0) {
      problems.push({
        severity: 'error',
        code: 'fetch_failure',
        kind: row.target,
        count: fetchError,
        message: `${row.target} has ${fetchError} permanent fetch failures`,
        score: 700_000 + fetchError,
      });
    }
    if (fetchTransient > 0) {
      problems.push({
        severity: 'warning',
        code: 'fetch_failure',
        kind: row.target,
        count: fetchTransient,
        message: `${row.target} has ${fetchTransient} transient fetch failures`,
        score: 400_000 + fetchTransient,
      });
    }
  }

  return problems;
}

function queueProblems(
  rows: CrawlerStatusRow[],
  workerJobs: IGetCrawlerWorkerJobStatusResult[],
) {
  const outstanding = (task: string) =>
    workerJobs.some(
      (job) =>
        job.task_identifier === task &&
        job.ready + job.delayed + job.locked > 0,
    );
  const processJobOutstanding = outstanding('frontier_process');
  const schedulerJobOutstanding = outstanding('frontier_schedule');
  const fetchJobOutstanding = outstanding('frontier_fetch');
  const problems: ScoredStatusProblem[] = [];

  for (const row of rows) {
    if (row.is_stale) {
      problems.push({
        severity: 'warning',
        code: 'stale_backlog',
        kind: row.target,
        host: row.host,
        count: row.fetch_due,
        message: `${row.target} backlog ETA breaches next_fetch_at + revalidatePeriod`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 500_000 + row.unscheduled_fetch,
      });
    } else if (row.unscheduled_fetch > 0 && row.host) {
      problems.push({
        severity: 'info',
        code: 'queue_pressure',
        kind: row.target,
        host: row.host,
        count: row.unscheduled_fetch,
        message: `${row.target} has due rows waiting behind the scheduler cap`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 100_000 + row.unscheduled_fetch,
      });
    }

    if (
      row.fetch_due > 0 &&
      !fetchJobOutstanding &&
      !schedulerJobOutstanding &&
      row.scheduled_fetch === 0
    ) {
      problems.push({
        severity: 'warning',
        code: 'scheduler_gap',
        kind: row.target,
        host: row.host,
        count: row.fetch_due,
        message: `${row.target} has due fetches but no outstanding frontier_fetch job`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 450_000 + row.fetch_due,
      });
    }

    if (
      row.process_ready > 0 &&
      !processJobOutstanding &&
      !fetchJobOutstanding &&
      !schedulerJobOutstanding
    ) {
      problems.push({
        severity: 'warning',
        code: 'process_gap',
        kind: row.target,
        count: row.process_ready,
        message: `${row.target} has process-ready rows but no outstanding frontier_process job`,
        sample_id: row.sample_process_id,
        at: row.oldest_process_ready_at,
        score: 420_000 + row.process_ready,
      });
    }
  }

  return problems;
}

function workerJobProblems(rows: IGetCrawlerWorkerJobStatusResult[]): ScoredStatusProblem[] {
  return rows
    .filter((row) => row.failed > 0)
    .map((row) => ({
      severity: 'error',
      code: 'worker_failure',
      task: row.task_identifier,
      count: row.failed,
      message: `${row.task_identifier} has ${row.failed} exhausted jobs`,
      at: row.latest_failed_at,
      score: 600_000 + row.failed,
    }));
}

function publicProblem({ score: _score, ...problem }: ScoredStatusProblem): StatusProblem {
  return problem;
}

export function buildStatusSnapshot(inputs: StatusInputs): StatusSnapshot {
  const scheduleByHost = new Map(
    inputs.scheduleRows.map((row) => [row.host, row]),
  );
  const frontiers = enrichFrontierRows(inputs.frontierRows);
  computeQueueEta(frontiers, scheduleByHost, inputs.generatedAt);

  const scoredProblems = [
    ...loaderProblems(frontiers),
    ...frontierProblems(frontiers),
    ...queueProblems(frontiers, inputs.workerJobs),
    ...workerJobProblems(inputs.workerJobs),
  ].sort(sortProblems);
  frontiers.sort(sortFrontiers);
  const problems = scoredProblems.map(publicProblem);

  return {
    generated_at: inputs.generatedAt.toISOString(),
    filter: inputs.filter,
    problems,
    frontiers,
    failures: inputs.failureGroups,
    worker_jobs: inputs.workerJobs,
    schedule: inputs.scheduleRows,
  };
}
