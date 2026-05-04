import type {
  IGetCrawlerBacklogStatusResult,
  IGetCrawlerControlJobStatusResult,
  IGetCrawlerFetchJobSummaryResult,
  IGetCrawlerScheduleStatusResult,
  IGetFrontierFailureGroupsResult,
  IGetFrontierKindStatusResult,
} from './crawler.queries.ts';

const DEFAULT_HOST_SPACING_MS = 50;

type StatusFilter = {
  federation?: string;
  kind?: string;
};

export type LoaderHealthMetadata = {
  target: string;
  has_loader: boolean;
  host: string | null;
  revalidate_period: string | null;
  revalidate_period_ms: number | null;
  error?: string;
};

type StatusInputs = {
  generated_at: Date;
  filter: StatusFilter;
  status_rows: IGetFrontierKindStatusResult[];
  backlog_rows: IGetCrawlerBacklogStatusResult[];
  failure_groups: IGetFrontierFailureGroupsResult[];
  fetch_job: IGetCrawlerFetchJobSummaryResult;
  control_jobs: IGetCrawlerControlJobStatusResult[];
  schedule_rows: IGetCrawlerScheduleStatusResult[];
  loader_metadata: LoaderHealthMetadata[];
};

type StatusSeverity = 'error' | 'warning' | 'info';

type StatusProblem = {
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
  score: number;
};

type EnrichedBacklogRow = IGetCrawlerBacklogStatusResult & {
  host: string | null;
  has_loader: boolean;
  revalidate_period: string | null;
  revalidate_period_ms: number | null;
  deadline_at: Date | null;
  eta_at: Date | null;
  is_stale: boolean;
};

type StatusSummary = {
  kinds: number;
  frontiers: number;
  fetch_due: number;
  unscheduled_fetch: number;
  process_ready: number;
  stale_kinds: number;
  unknown_loaders: number;
  fetch_errors: number;
  fetch_transient: number;
  process_errors: number;
  fetch_job_failures: number;
};

type StatusSnapshot = {
  generated_at: string;
  filter: StatusFilter;
  summary: StatusSummary;
  problems: StatusProblem[];
  frontiers: IGetFrontierKindStatusResult[];
  backlog: EnrichedBacklogRow[];
  failures: IGetFrontierFailureGroupsResult[];
  fetch_job: IGetCrawlerFetchJobSummaryResult;
  control_jobs: IGetCrawlerControlJobStatusResult[];
  schedule: IGetCrawlerScheduleStatusResult[];
  suggested_commands: string[];
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

function sortProblems(a: StatusProblem, b: StatusProblem) {
  return (
    severityRank(b.severity) - severityRank(a.severity) ||
    b.score - a.score ||
    (a.at?.getTime() ?? Number.MAX_SAFE_INTEGER) -
      (b.at?.getTime() ?? Number.MAX_SAFE_INTEGER) ||
    String(a.kind ?? a.task ?? '').localeCompare(String(b.kind ?? b.task ?? ''))
  );
}

function sortBacklog(a: EnrichedBacklogRow, b: EnrichedBacklogRow) {
  return (
    Number(b.is_stale) - Number(a.is_stale) ||
    (b.unscheduled_fetch ?? 0) - (a.unscheduled_fetch ?? 0) ||
    (b.fetch_due ?? 0) - (a.fetch_due ?? 0) ||
    (a.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) -
      (b.oldest_due_at?.getTime() ?? Number.MAX_SAFE_INTEGER) ||
    kindId(a.federation, a.kind).localeCompare(kindId(b.federation, b.kind))
  );
}

export function parseIntervalMs(value: string | null | undefined) {
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

function makeSuggestedCommands(
  filter: StatusFilter,
  failures: IGetFrontierFailureGroupsResult[],
  problems: StatusProblem[],
) {
  const scope =
    filter.federation && filter.kind
      ? `${filter.federation}:${filter.kind}`
      : filter.federation;
  const scoped = (command: string) => (scope ? `${command} ${scope}` : command);
  const commands = [
    scoped('pnpm crawler status') + ' --problems',
    scoped('pnpm crawler failures'),
    scoped('pnpm crawler failures') + ' --group',
  ];

  if (problems.some((problem) => problem.code === 'worker_failure')) {
    commands.push(
      scope ? `pnpm crawler jobs --failed --target ${scope}` : 'pnpm crawler jobs --failed',
    );
  }
  if (
    problems.some(
      (problem) => problem.code === 'scheduler_gap' || problem.code === 'process_gap',
    )
  ) {
    commands.push('pnpm crawler kick');
  }

  const sampleProblem = problems.find((problem) => problem.sample_id);
  if (sampleProblem?.sample_id) {
    commands.push(`pnpm crawler explain ${sampleProblem.sample_id}`);
  }

  const failureSample = failures.find((failure) => failure.sample_id);
  if (failureSample?.sample_id && failureSample.sample_id !== sampleProblem?.sample_id) {
    commands.push(`pnpm crawler explain ${failureSample.sample_id}`);
  }

  const failure = failures.find((row) => row.federation && row.kind);
  if (failure) {
    const target = `${failure.federation}:${failure.kind}`;
    const http = failure.http_status == null ? '' : ` --http-status ${failure.http_status}`;
    commands.push(`pnpm crawler refetch ${target} --failed${http}`);
  }

  const refetchProblem = problems.find(
    (problem) =>
      problem.kind &&
      problem.host &&
      problem.sample_key != null &&
      problem.code !== 'unknown_loader',
  );
  if (refetchProblem?.kind && refetchProblem.sample_key != null) {
    const target = refetchProblem.sample_key
      ? `${refetchProblem.kind}:${refetchProblem.sample_key}`
      : refetchProblem.kind;
    commands.push(`pnpm crawler refetch ${target}`);
  }

  return [...new Set(commands)];
}

function attachLoaderMetadata(
  rows: IGetCrawlerBacklogStatusResult[],
  loaders: Map<string, LoaderHealthMetadata>,
): EnrichedBacklogRow[] {
  return rows.map((row) => {
    const loader = loaders.get(kindId(row.federation, row.kind));
    const deadline_at =
      row.oldest_due_at && loader?.revalidate_period_ms
        ? new Date(row.oldest_due_at.getTime() + loader.revalidate_period_ms)
        : null;

    return {
      ...row,
      host: loader?.host ?? null,
      has_loader: Boolean(loader?.has_loader),
      revalidate_period: loader?.revalidate_period ?? null,
      revalidate_period_ms: loader?.revalidate_period_ms ?? null,
      deadline_at,
      eta_at: null,
      is_stale: false,
    };
  });
}

function computeBacklogEta(
  rows: EnrichedBacklogRow[],
  schedules: Map<string, IGetCrawlerScheduleStatusResult>,
  now: Date,
) {
  const byHost = new Map<string, EnrichedBacklogRow[]>();
  for (const row of rows) {
    if (!row.host || (row.fetch_due ?? 0) <= 0) continue;
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
      row.eta_at = new Date(tail.getTime() + (row.unscheduled_fetch ?? 0) * spacing);
      row.is_stale = Boolean(row.deadline_at && row.eta_at > row.deadline_at);
      tail = row.eta_at;
    }
  }
}

function loaderProblems(loaders: LoaderHealthMetadata[]): StatusProblem[] {
  const problems: StatusProblem[] = [];
  for (const loader of loaders) {
    if (!loader.has_loader) {
      problems.push({
        severity: 'error',
        code: 'unknown_loader',
        kind: loader.target,
        count: 1,
        message: `${loader.target} has frontier rows but no loader`,
        score: 1_000_000,
      });
      continue;
    }
    if (!loader.error) continue;

    problems.push({
      severity: 'error',
      code: 'loader_request_error',
      kind: loader.target,
      count: 1,
      message: `${loader.target} loader request failed while deriving status metadata: ${loader.error}`,
      score: 900_000,
    });
  }
  return problems;
}

function statusProblems(rows: IGetFrontierKindStatusResult[]): StatusProblem[] {
  const problems: StatusProblem[] = [];
  for (const row of rows) {
    const id = kindId(row.federation, row.kind);
    const fetchError = row.fetch_error ?? 0;
    const fetchTransient = row.fetch_transient ?? 0;
    const processError = row.process_error ?? 0;

    if (processError > 0) {
      problems.push({
        severity: 'error',
        code: 'process_failure',
        kind: id,
        count: processError,
        message: `${id} has ${processError} processing failures`,
        score: 800_000 + processError,
      });
    }
    if (fetchError > 0) {
      problems.push({
        severity: 'error',
        code: 'fetch_failure',
        kind: id,
        count: fetchError,
        message: `${id} has ${fetchError} permanent fetch failures`,
        score: 700_000 + fetchError,
      });
    }
    if (fetchTransient > 0) {
      problems.push({
        severity: 'warning',
        code: 'fetch_failure',
        kind: id,
        count: fetchTransient,
        message: `${id} has ${fetchTransient} transient fetch failures`,
        score: 400_000 + fetchTransient,
      });
    }
  }

  return problems;
}

function backlogProblems(
  rows: EnrichedBacklogRow[],
  fetchJob: IGetCrawlerFetchJobSummaryResult,
  controlJobs: IGetCrawlerControlJobStatusResult[],
) {
  const processJobOutstanding = controlJobs.some(
    (job) =>
      job.task_identifier === 'frontier_process' &&
      (job.ready ?? 0) + (job.delayed ?? 0) + (job.locked ?? 0) > 0,
  );
  const schedulerJobOutstanding = controlJobs.some(
    (job) =>
      job.task_identifier === 'frontier_schedule' &&
      (job.ready ?? 0) + (job.delayed ?? 0) + (job.locked ?? 0) > 0,
  );
  const fetchJobOutstanding =
    (fetchJob.ready ?? 0) + (fetchJob.delayed ?? 0) + (fetchJob.locked ?? 0) > 0;
  const problems: StatusProblem[] = [];

  for (const row of rows) {
    const id = kindId(row.federation, row.kind);
    if (row.is_stale) {
      problems.push({
        severity: 'warning',
        code: 'stale_backlog',
        kind: id,
        host: row.host,
        count: row.fetch_due ?? 0,
        message: `${id} backlog ETA breaches next_fetch_at + revalidatePeriod`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 500_000 + (row.unscheduled_fetch ?? 0),
      });
    } else if ((row.unscheduled_fetch ?? 0) > 0 && row.host) {
      problems.push({
        severity: 'info',
        code: 'queue_pressure',
        kind: id,
        host: row.host,
        count: row.unscheduled_fetch ?? 0,
        message: `${id} has due rows waiting behind the scheduler cap`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 100_000 + (row.unscheduled_fetch ?? 0),
      });
    }

    if (
      (row.fetch_due ?? 0) > 0 &&
      !fetchJobOutstanding &&
      !schedulerJobOutstanding &&
      (row.scheduled_fetch ?? 0) === 0
    ) {
      problems.push({
        severity: 'warning',
        code: 'scheduler_gap',
        kind: id,
        host: row.host,
        count: row.fetch_due ?? 0,
        message: `${id} has due fetches but no outstanding frontier_fetch job`,
        sample_id: row.sample_due_id,
        sample_key: row.sample_due_key,
        at: row.oldest_due_at,
        score: 450_000 + (row.fetch_due ?? 0),
      });
    }

    if (
      (row.process_ready ?? 0) > 0 &&
      !processJobOutstanding &&
      !fetchJobOutstanding &&
      !schedulerJobOutstanding
    ) {
      problems.push({
        severity: 'warning',
        code: 'process_gap',
        kind: id,
        count: row.process_ready ?? 0,
        message: `${id} has process-ready rows but no outstanding frontier_process job`,
        sample_id: row.sample_process_id,
        at: row.oldest_process_ready_at,
        score: 420_000 + (row.process_ready ?? 0),
      });
    }
  }

  return problems;
}

function fetchJobProblems(row: IGetCrawlerFetchJobSummaryResult): StatusProblem[] {
  if ((row.failed ?? 0) === 0) return [];
  return [{
    severity: 'error',
    code: 'worker_failure',
    task: 'frontier_fetch',
    count: row.failed ?? 0,
    message: `frontier_fetch has ${row.failed ?? 0} exhausted jobs`,
    at: row.latest_update_at,
    score: 600_000 + (row.failed ?? 0),
  }];
}

function controlJobProblems(rows: IGetCrawlerControlJobStatusResult[]): StatusProblem[] {
  return rows
    .filter((row) => (row.failed ?? 0) > 0)
    .map((row) => ({
      severity: 'error',
      code: 'worker_failure',
      task: row.task_identifier,
      count: row.failed ?? 0,
      message: `${row.task_identifier} has ${row.failed ?? 0} exhausted jobs`,
      at: row.latest_update_at,
      score: 600_000 + (row.failed ?? 0),
    }));
}

function buildSummary(
  statusRows: IGetFrontierKindStatusResult[],
  backlog: EnrichedBacklogRow[],
  loaders: LoaderHealthMetadata[],
  fetchJob: IGetCrawlerFetchJobSummaryResult,
): StatusSummary {
  return {
    kinds: statusRows.length,
    frontiers: statusRows.reduce((sum, row) => sum + (row.total ?? 0), 0),
    fetch_due: backlog.reduce((sum, row) => sum + (row.fetch_due ?? 0), 0),
    unscheduled_fetch: backlog.reduce((sum, row) => sum + (row.unscheduled_fetch ?? 0), 0),
    process_ready: backlog.reduce((sum, row) => sum + (row.process_ready ?? 0), 0),
    stale_kinds: backlog.filter((row) => row.is_stale).length,
    unknown_loaders: loaders.filter((row) => !row.has_loader).length,
    fetch_errors: statusRows.reduce((sum, row) => sum + (row.fetch_error ?? 0), 0),
    fetch_transient: statusRows.reduce((sum, row) => sum + (row.fetch_transient ?? 0), 0),
    process_errors: statusRows.reduce((sum, row) => sum + (row.process_error ?? 0), 0),
    fetch_job_failures: fetchJob.failed ?? 0,
  };
}

export function buildStatusSnapshot(inputs: StatusInputs): StatusSnapshot {
  const loaderByKind = new Map(
    inputs.loader_metadata.map((row) => [row.target, row]),
  );
  const scheduleByHost = new Map(
    inputs.schedule_rows
      .filter((row) => row.host)
      .map((row) => [row.host!, row]),
  );
  const backlog = attachLoaderMetadata(inputs.backlog_rows, loaderByKind);
  computeBacklogEta(backlog, scheduleByHost, inputs.generated_at);

  const problems = [
    ...loaderProblems(inputs.loader_metadata),
    ...statusProblems(inputs.status_rows),
    ...backlogProblems(backlog, inputs.fetch_job, inputs.control_jobs),
    ...fetchJobProblems(inputs.fetch_job),
    ...controlJobProblems(inputs.control_jobs),
  ].sort(sortProblems);
  backlog.sort(sortBacklog);

  return {
    generated_at: inputs.generated_at.toISOString(),
    filter: inputs.filter,
    summary: buildSummary(
      inputs.status_rows,
      backlog,
      inputs.loader_metadata,
      inputs.fetch_job,
    ),
    problems,
    frontiers: inputs.status_rows,
    backlog,
    failures: inputs.failure_groups,
    fetch_job: inputs.fetch_job,
    control_jobs: inputs.control_jobs,
    schedule: inputs.schedule_rows,
    suggested_commands: makeSuggestedCommands(inputs.filter, inputs.failure_groups, problems),
  };
}
