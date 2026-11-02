'use client';

import { Table } from '@/ui/Table';
import { type ComponentProps, Fragment, useCallback, useEffect, useState } from 'react';
import Link from 'next/link';
import { parseAsInteger, parseAsString, useQueryStates } from 'nuqs';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { useQuery } from 'urql';
import {
  CrawlerOverviewDocument,
  type CrawlerOverviewQuery,
  CrawlerTargetsDocument,
  type CrawlerTargetsQuery,
} from '@/graphql/Crawler';
import { TextField } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { SubmitButton } from '@/ui/submit';
import { PageHeader } from '@/ui/PageHeader';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { numericFullFormatter } from '@/ui/format';
import { ChevronRight } from 'lucide-react';
import { CrawlerActions } from './CrawlerActions';

export function Crawler({ allowRefetch }: { allowRefetch: boolean }) {
  const [filters, setFilters] = useQueryStates(
    {
      federation: parseAsString.withDefault(''),
      kind: parseAsString.withDefault(''),
      key: parseAsString.withDefault(''),
      all: parseAsString.withDefault(''),
      page: parseAsInteger,
    },
    { shallow: true, scroll: false },
  );
  const [search, setSearch] = useState({
    federation: filters.federation,
    kind: filters.kind,
    key: filters.key,
  });
  useEffect(() => {
    const timeout = setTimeout(
      () =>
        setSearch({
          federation: filters.federation,
          kind: filters.kind,
          key: filters.key,
        }),
      300,
    );
    return () => clearTimeout(timeout);
  }, [filters.federation, filters.kind, filters.key]);
  const pending =
    search.federation !== filters.federation ||
    search.kind !== filters.kind ||
    search.key !== filters.key;
  const target =
    search.federation && search.kind ? `${search.federation}:${search.kind}` : null;
  const [overview, refreshOverview] = useQuery({
    query: CrawlerOverviewDocument,
    variables: {
      federation: search.federation || undefined,
      kind: search.kind || undefined,
    },
    pause: !!target || pending,
  });
  const targets = usePaginatedQuery({
    query: CrawlerTargetsDocument,
    pause: !target || pending,
    variables: (offset) => ({
      condition: {
        federation: search.federation,
        kind: search.kind,
        keyContains: search.key,
      },
      offset,
    }),
    limit: 50,
    getTotal: (data) => data.crawlerTargets?.totalCount,
  });
  const error = target ? targets.error : overview.error;
  const loading = pending || (target ? targets.fetching : overview.fetching);
  const reexecuteQuery = target ? targets.reexecuteQuery : refreshOverview;
  const refresh = useCallback(
    () => reexecuteQuery({ requestPolicy: 'network-only' }),
    [reexecuteQuery],
  );
  useEffect(() => {
    if (loading) return;
    const interval = setInterval(() => {
      if (document.visibilityState === 'visible') refresh();
    }, 1000);
    return () => clearInterval(interval);
  }, [loading, refresh]);
  return (
    <>
      <PageHeader
        title={target || 'Crawler'}
        breadcrumbs={target ? [{ label: 'Crawler', href: '/admin/crawler' }] : undefined}
      >
        {target && !pending && (
          <CrawlerActions
            key={`${target}:${search.key}`}
            federation={search.federation}
            kind={search.kind}
            keyContains={search.key}
            onQueued={refresh}
          />
        )}
      </PageHeader>
      <div className="flex flex-wrap items-end gap-3">
        <TextField
          name="federation"
          label="federation"
          className="w-32"
          value={filters.federation}
          onChange={(event) =>
            setFilters({ federation: event.target.value, key: null, page: null })
          }
        />
        <TextField
          name="kind"
          label="kind"
          className="w-48"
          value={filters.kind}
          onChange={(event) =>
            setFilters({ kind: event.target.value, key: null, page: null })
          }
        />
        {filters.federation && filters.kind ? (
          <TextField
            name="key"
            label="key"
            className="w-48"
            value={filters.key}
            onChange={(event) => setFilters({ key: event.target.value, page: null })}
          />
        ) : (
          <Checkbox
            name="all"
            label="Show completed"
            checked={filters.all === '1'}
            onChange={(event) =>
              setFilters({ all: event.currentTarget.checked ? '1' : '' })
            }
          />
        )}
        <SubmitButton
          type="button"
          size="sm"
          variant="outline"
          className="min-w-16"
          aria-label="Refresh"
          aria-busy={loading}
          state={loading ? 'loading' : 'not-requested'}
          onClick={refresh}
        >
          Refresh
        </SubmitButton>
      </div>
      {target && (
        <p className="my-2 text-sm text-neutral-11">{targets.pagination.total} targets</p>
      )}
      {error ? (
        <FormError error={error} />
      ) : target ? (
        targets.data?.crawlerTargets ? (
          <Targets
            rows={targets.data.crawlerTargets.nodes}
            pagination={targets.pagination}
          />
        ) : null
      ) : overview.data ? (
        <Overview
          {...overview.data}
          all={filters.all}
          allowRefetch={allowRefetch}
          onQueued={refresh}
        />
      ) : null}
    </>
  );
}

function Targets({
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
                href={`/admin/crawler/${row.id}`}
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
                      href={{
                        pathname: '/admin/crawler',
                        query: { federation: row.federation, kind: row.kind },
                      }}
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
                        href={{
                          pathname: '/admin/crawler',
                          query: { federation, kind: row.kind },
                        }}
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
