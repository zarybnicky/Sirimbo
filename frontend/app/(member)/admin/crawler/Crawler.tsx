'use client';

import { Table } from '@/ui/Table';
import {
  type ComponentProps,
  type ReactNode,
  Fragment,
  useEffect,
  useState,
} from 'react';
import Link from 'next/link';
import {
  parseAsArrayOf,
  parseAsInteger,
  parseAsString,
  parseAsStringLiteral,
  useQueryState,
  useQueryStates,
} from 'nuqs';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { useQuery, type UseQueryExecute } from 'urql';
import { toast } from 'react-toastify';
import {
  CrawlerOverviewDocument,
  type CrawlerOverviewQuery,
  CrawlerJobsDocument,
  CrawlerTargetsDocument,
  type CrawlerTargetsQuery,
} from '@/graphql/Crawler';
import { FacetFilter } from '@/ui/FacetFilter';
import { ListHeader } from '@/ui/ListHeader';
import { PageHeader } from '@/ui/PageHeader';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { numericFullFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { ActionGroup } from '@/ui/ActionGroup';
import { Check, ChevronRight, ListTodo, RefreshCw } from 'lucide-react';
import { CrawlerActions } from './CrawlerActions';
import { completeCrawlerJob } from './actions';

function Refresh({
  loading,
  reexecuteQuery,
}: {
  loading: boolean;
  reexecuteQuery: UseQueryExecute;
}) {
  useEffect(() => {
    if (loading) return;
    const interval = setInterval(() => {
      if (document.visibilityState === 'visible')
        reexecuteQuery({ requestPolicy: 'network-only' });
    }, 1000);
    return () => clearInterval(interval);
  }, [loading, reexecuteQuery]);
  return (
    <button
      type="button"
      aria-label="Refresh"
      title="Refresh"
      aria-busy={loading}
      onClick={() => reexecuteQuery({ requestPolicy: 'network-only' })}
      className="rounded-sm p-1 text-neutral-11 hover:text-neutral-12 focus-visible:outline-accent-9"
    >
      <RefreshCw
        aria-hidden="true"
        className={loading ? 'size-4 animate-spin' : 'size-4'}
      />
    </button>
  );
}

export function Crawler({
  allowRefetch,
  children,
}: {
  allowRefetch: boolean;
  children?: ReactNode;
}) {
  const [filters, setFilters] = useQueryStates(
    {
      federation: parseAsArrayOf(parseAsString).withDefault([]),
      all: parseAsString.withDefault(''),
    },
    { shallow: true, scroll: false },
  );
  const [overview, reexecuteQuery] = useQuery({ query: CrawlerOverviewDocument });
  const rows = overview.data?.rows ?? [];
  const federations = [
    ...new Set(rows.map((row) => row.federation).filter((value) => value != null)),
  ].toSorted();
  const matches = (row: { federation: string | null }) =>
    filters.federation.length === 0 || filters.federation.includes(row.federation ?? '');
  const filteredRows = rows.filter(matches);

  return (
    <>
      <PageHeader
        title="Crawler"
        primary="jobs"
        actions={[
          {
            id: 'jobs',
            key: 'jobs',
            label: 'Jobs',
            icon: ListTodo,
            href: '/admin/crawler/jobs',
          },
        ]}
      >
        <Refresh loading={overview.fetching} reexecuteQuery={reexecuteQuery} />
      </PageHeader>
      <ListHeader
        collapsible={false}
        pagination={
          overview.data && !overview.error
            ? { total: filteredRows.length, page: 1, limit: filteredRows.length }
            : undefined
        }
        actions={
          <button
            type="button"
            aria-pressed={filters.all === '1'}
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setFilters({ all: filters.all === '1' ? null : '1' })}
          >
            Show completed
          </button>
        }
      >
        <FacetFilter
          inline
          label="Federation"
          value={filters.federation}
          options={federations.map((value) => ({
            value,
            label: value,
            count: rows.filter((row) => row.federation === value).length,
          }))}
          onChange={(federation) => setFilters({ federation })}
        />
      </ListHeader>
      {overview.error ? (
        <FormError error={overview.error} />
      ) : overview.data ? (
        <Overview
          rows={filteredRows}
          failures={(overview.data.failures ?? []).filter(matches)}
          all={filters.all}
          allowRefetch={allowRefetch}
          onQueued={() => reexecuteQuery({ requestPolicy: 'network-only' })}
        />
      ) : null}

      <div className="col-feature my-4">
        {children}
      </div>
    </>
  );
}

const fetchStatuses = ['PENDING', 'OK', 'ERROR', 'TRANSIENT', 'GONE'] as const;
const processStatuses = ['PENDING', 'OK', 'ERROR'] as const;

export function CrawlerTargets({
  federation,
  kind,
}: {
  federation: string;
  kind: string;
}) {
  const [filters, setFilters] = useQueryStates(
    {
      key: parseAsString.withDefault(''),
      fetchStatus: parseAsStringLiteral(fetchStatuses),
      processStatus: parseAsStringLiteral(processStatuses),
      page: parseAsInteger,
    },
    { shallow: true, scroll: false },
  );
  const [key, setKey] = useState(filters.key);
  useEffect(() => {
    const timeout = setTimeout(() => setKey(filters.key), 300);
    return () => clearTimeout(timeout);
  }, [filters.key]);
  const pending = key !== filters.key;
  const targets = usePaginatedQuery({
    query: CrawlerTargetsDocument,
    pause: pending,
    variables: (offset) => ({
      condition: {
        federation,
        kind,
        keyContains: key,
        fetchStatus: filters.fetchStatus ?? undefined,
        processStatus: filters.processStatus ?? undefined,
      },
      offset,
    }),
    limit: 50,
    getTotal: (data) => data.crawlerTargets?.totalCount,
  });
  const loading = pending || targets.fetching;

  return (
    <>
      <PageHeader
        title={federation + ':' + kind}
        breadcrumbs={[{ title: 'Crawler', href: '/admin/crawler' }]}
      >
        {!pending && (
          <CrawlerActions
            key={key}
            federation={federation}
            kind={kind}
            keyContains={key}
            onQueued={() => targets.reexecuteQuery({ requestPolicy: 'network-only' })}
          />
        )}
        <Refresh loading={loading} reexecuteQuery={targets.reexecuteQuery} />
      </PageHeader>
      <ListHeader
        collapsible={false}
        pagination={!targets.error ? targets.pagination : undefined}
        search={filters.key}
        onSearchChange={(key) => setFilters({ key, page: null })}
        searchPlaceholder="Frontier key…"
      >
        <FacetFilter
          inline
          label="Fetch status"
          value={filters.fetchStatus ? [filters.fetchStatus] : []}
          options={fetchStatuses.map((value) => ({
            value,
            label: value.toLowerCase(),
          }))}
          onChange={(statuses) =>
            setFilters({ fetchStatus: statuses.at(-1) ?? null, page: null })
          }
        />
        <FacetFilter
          inline
          label="Process status"
          value={filters.processStatus ? [filters.processStatus] : []}
          options={processStatuses.map((value) => ({
            value,
            label: value.toLowerCase(),
          }))}
          onChange={(statuses) =>
            setFilters({ processStatus: statuses.at(-1) ?? null, page: null })
          }
        />
      </ListHeader>
      {targets.error ? (
        <FormError error={targets.error} />
      ) : targets.data?.crawlerTargets ? (
        <TargetTable
          rows={targets.data.crawlerTargets.nodes}
          pagination={targets.pagination}
        />
      ) : null}
    </>
  );
}

const jobStates = ['queued', 'locked', 'failed'];

export function CrawlerJobs() {
  const [states, setStates] = useQueryState(
    'state',
    parseAsArrayOf(parseAsStringLiteral(jobStates)).withDefault(jobStates),
  );
  const [result, reexecuteQuery] = useQuery({ query: CrawlerJobsDocument });
  const jobs = result.data?.crawlerFetchJobs?.nodes ?? [];
  const rows = jobs.filter((row) => states.includes(row.state ?? ''));

  return (
    <>
      <PageHeader
        title="Crawler jobs"
        breadcrumbs={[{ title: 'Crawler', href: '/admin/crawler' }]}
      >
        <Refresh loading={result.fetching} reexecuteQuery={reexecuteQuery} />
      </PageHeader>
      <ListHeader
        collapsible={false}
        pagination={
          result.data && !result.error
            ? { total: rows.length, page: 1, limit: rows.length }
            : undefined
        }
      >
        <FacetFilter
          inline
          label="State"
          value={states}
          defaultValue={jobStates}
          options={jobStates.map((value) => ({
            value,
            label:
              value === 'locked'
                ? 'Running'
                : value.charAt(0).toUpperCase() + value.slice(1),
            count: jobs.filter((row) => row.state === value).length,
          }))}
          onChange={setStates}
        />
      </ListHeader>
      <Table
        columns={[
          'state',
          'job',
          'target',
          'attempts',
          'scheduled',
          'locked since',
          'worker error',
        ]}
        className="col-full-width whitespace-nowrap"
        empty="No fetch jobs"
        busy={result.fetching && !result.data}
        error={result.error}
      >
        {rows.map((row) => (
          <tr key={row.jobId} className="border-t border-neutral-4 align-top">
            <td>{row.state === 'locked' ? 'running' : row.state}</td>
            <td>
              <div className="flex items-center gap-2">
                <ActionGroup
                  primary="complete"
                  iconOnly
                  actions={[
                    {
                      id: 'complete',
                      key: row.jobId!,
                      label: 'Mark complete',
                      icon: Check,
                      execute: async () => {
                        try {
                          await completeCrawlerJob(row.jobId!);
                          reexecuteQuery({ requestPolicy: 'network-only' });
                        } catch (error) {
                          toast.error(
                            error instanceof Error
                              ? error.message
                              : 'Could not complete job.',
                          );
                        }
                      },
                    },
                  ]}
                />
                {row.jobId}
              </div>
            </td>
            <th scope="row" className="font-mono">
              <Link
                className="text-accent-11 underline"
                href={`/admin/crawler/${row.federation}:${row.kind}:${encodeURIComponent(row.frontierKey ?? '')}`}
                prefetch={false}
              >
                {row.federation}:{row.kind}
                {row.frontierKey ? ':' + row.frontierKey : ''}
              </Link>
            </th>
            <td>
              {row.attempts ?? '-'} / {row.maxAttempts ?? '-'}
            </td>
            <td>{row.runAt ? numericFullFormatter.format(new Date(row.runAt)) : '-'}</td>
            <td>
              {row.lockedAt ? numericFullFormatter.format(new Date(row.lockedAt)) : '-'}
            </td>
            <td className="min-w-64 whitespace-normal break-all">
              {row.jobError && (
                <details>
                  <summary className="cursor-pointer">Error</summary>
                  <pre className="max-h-72 overflow-auto whitespace-pre-wrap">
                    {row.jobError}
                  </pre>
                </details>
              )}
            </td>
          </tr>
        ))}
      </Table>
    </>
  );
}

function TargetTable({
  rows,
  pagination,
}: {
  rows: NonNullable<CrawlerTargetsQuery['crawlerTargets']>['nodes'];
  pagination: ComponentProps<typeof Pagination>;
}) {
  return (
    <>
      <Table
        columns={[
          'key',
          'fetch status',
          'process status',
          'error count',
          'last fetched',
          'next fetch',
        ]}
        className="col-full-width whitespace-nowrap"
      >
        {rows.map((row) => (
          <tr key={row.id} className="border-t border-neutral-4">
            <th scope="row" className="font-mono">
              <Link
                className="text-accent-11 underline"
                href={`/admin/crawler/${row.federation}:${row.kind}:${encodeURIComponent(row.key)}`}
                prefetch={false}
              >
                {row.key || '(root)'}
              </Link>
            </th>
            <td>{row.fetchStatus.toLowerCase()}</td>
            <td>{row.processStatus.toLowerCase()}</td>
            <td>{row.errorCount || ''}</td>
            <td>
              {row.lastFetchedAt
                ? numericFullFormatter.format(new Date(row.lastFetchedAt))
                : '-'}
            </td>
            <td>{numericFullFormatter.format(new Date(row.nextFetchAt))}</td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}

function Overview({
  rows,
  failures,
  all,
  allowRefetch,
  onQueued,
}: CrawlerOverviewQuery & { all: string; allowRefetch: boolean; onQueued: () => void }) {
  const [expanded, setExpanded] = useState<Set<string>>(() => new Set());
  const failuresByTarget = Object.groupBy(
    failures ?? [],
    (row): string => `${row.federation}:${row.kind}`,
  );
  const frontiers = (rows ?? [])
    .map((row) => {
      const target = `${row.federation}:${row.kind}`;
      const failures = failuresByTarget[target] ?? [];
      const fetchDue = (row.fetchDue ?? 0) + (allowRefetch ? (row.refetchDue ?? 0) : 0);
      return {
        ...row,
        fetchDue,
        target,
        failures,
        stable:
          row.total === row.done &&
          !fetchDue &&
          !row.fetchError &&
          !row.fetchTransient &&
          !row.processError &&
          failures.length === 0,
      };
    })
    .toSorted(
      (a, b) =>
        (b.processError ?? 0) - (a.processError ?? 0) ||
        (b.fetchError ?? 0) - (a.fetchError ?? 0) ||
        (b.fetchTransient ?? 0) - (a.fetchTransient ?? 0) ||
        (b.processReady ?? 0) - (a.processReady ?? 0) ||
        b.fetchDue - a.fetchDue ||
        (b.fetchPending ?? 0) - (a.fetchPending ?? 0) ||
        (a.federation ?? '').localeCompare(b.federation ?? '') ||
        (a.kind ?? '').localeCompare(b.kind ?? ''),
    );
  const done = Object.groupBy(
    frontiers.filter((row) => row.stable),
    (row) => row.federation ?? '',
  );

  return (
    <>
      <Table
        columns={[
          'target',
          'done',
          'due',
          'fetch errors',
          'process errors',
          'process',
          'last fetched',
        ]}
        className="col-feature whitespace-nowrap"
        empty=""
        footer={
          <tr className="border-t border-neutral-4">
            <th scope="row">{frontiers.length} total</th>
            <td>{frontiers.reduce((sum, row) => sum + (row.done ?? 0), 0) || ''}</td>
            <td>{frontiers.reduce((sum, row) => sum + (row.fetchDue ?? 0), 0) || ''}</td>
            <td colSpan={4} />
          </tr>
        }
      >
        {frontiers
          .filter((row) => all === '1' || !row.stable)
          .map((row) => (
            <Fragment key={row.target}>
              <tr className="border-t border-neutral-4">
                <th scope="row" className="font-mono">
                  <div className="flex items-center gap-2 pl-2">
                    {row.failures.length > 0 ? (
                      <button
                        type="button"
                        aria-label={`Failures for ${row.target}`}
                        aria-expanded={expanded.has(row.target)}
                        aria-controls={`failures-${row.target}`}
                        className="rounded-sm text-neutral-11 hover:text-neutral-12 focus-visible:outline-accent-9"
                        onClick={() =>
                          setExpanded((previous) => {
                            const next = new Set(previous);
                            if (next.has(row.target)) next.delete(row.target);
                            else next.add(row.target);
                            return next;
                          })
                        }
                      >
                        <ChevronRight
                          aria-hidden="true"
                          className={
                            expanded.has(row.target) ? 'size-4 rotate-90' : 'size-4'
                          }
                        />
                      </button>
                    ) : (
                      <span className="size-4" aria-hidden="true" />
                    )}
                    <Link
                      className="text-accent-11 underline"
                      href={`/admin/crawler/${row.target}`}
                      prefetch={false}
                    >
                      {row.target}
                    </Link>
                  </div>
                </th>
                <td>{row.done || ''}</td>
                <td>{row.fetchDue || ''}</td>
                <td>
                  {row.fetchError || ''}
                  {!!row.fetchTransient &&
                    (row.fetchError
                      ? ` (+${row.fetchTransient} temp)`
                      : `${row.fetchTransient} temp`)}
                </td>
                <td>{row.processError || ''}</td>
                <td>{row.processReady || ''}</td>
                <td>
                  {row.latest ? numericFullFormatter.format(new Date(row.latest)) : '-'}
                </td>
              </tr>
              {row.failures.length > 0 && (
                <tr id={`failures-${row.target}`} hidden={!expanded.has(row.target)}>
                  <td colSpan={7}>
                    <Table
                      columns={[
                        row.federation && row.kind ? (
                          <CrawlerActions
                            key="actions"
                            federation={row.federation}
                            kind={row.kind}
                            primary
                            onQueued={onQueued}
                          />
                        ) : (
                          'count'
                        ),
                        'failure',
                        'http',
                        'details',
                      ]}
                      className="whitespace-normal"
                    >
                      {row.failures.map((failure) => (
                        <tr
                          key={JSON.stringify([
                            failure.failure,
                            failure.httpStatus,
                            failure.errorFingerprint,
                          ])}
                          className="border-t border-neutral-4 align-top"
                        >
                          <td>
                            <div className="flex items-center gap-2">
                              {failure.federation &&
                                failure.kind &&
                                failure.failure &&
                                failure.errorFingerprint != null && (
                                  <CrawlerActions
                                    federation={failure.federation}
                                    kind={failure.kind}
                                    failureGroup={{
                                      failure: failure.failure,
                                      httpStatus: failure.httpStatus ?? null,
                                      errorFingerprint: failure.errorFingerprint,
                                    }}
                                    onQueued={onQueued}
                                  />
                                )}
                              {failure.count}
                            </div>
                          </td>
                          <td>{failure.failure}</td>
                          <td>{failure.httpStatus}</td>
                          <td className="break-all">{failure.errorFingerprint}</td>
                        </tr>
                      ))}
                    </Table>
                  </td>
                </tr>
              )}
            </Fragment>
          ))}
      </Table>
      {all !== '1' && (
        <section>
          <h2 className="mt-4 mb-2 text-lg font-bold">Done</h2>
          {Object.entries(done)
            .toSorted(([a], [b]) => a.localeCompare(b))
            .map(([federation, targets]) => (
              <p key={federation}>
                <strong>{federation}:</strong>{' '}
                {targets
                  ?.toSorted(
                    (a, b) =>
                      (b.total ?? 0) - (a.total ?? 0) ||
                      (a.kind ?? '').localeCompare(b.kind ?? ''),
                  )
                  .map((row, index) => (
                    <Fragment key={row.kind}>
                      {index > 0 && ', '}
                      <Link
                        className="text-accent-11 underline"
                        href={`/admin/crawler/${federation}:${row.kind}`}
                        prefetch={false}
                      >
                        {row.total === 1 ? row.kind : `${row.kind} (${row.total})`}
                      </Link>
                    </Fragment>
                  ))}
              </p>
            ))}
        </section>
      )}
    </>
  );
}
