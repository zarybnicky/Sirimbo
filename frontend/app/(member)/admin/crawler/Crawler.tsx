'use client';

import { Table } from '@/ui/Table';
import { Fragment, useEffect, useState, type ComponentProps } from 'react';
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
import { buttonCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { numericFullFormatter } from '@/ui/format';

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
    variables: { federation: search.federation || undefined, kind: search.kind || undefined },
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
  return (
    <>
      <PageHeader
        title={target || 'Crawler'}
        breadcrumbs={
          target
            ? [{ label: 'Crawler', href: '/admin/crawler' }, { label: target }]
            : undefined
        }
      />
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
        <button
          type="button"
          className={buttonCls({ variant: 'outline' })}
          onClick={() =>
            target
              ? targets.reexecuteQuery({ requestPolicy: 'network-only' })
              : refreshOverview({ requestPolicy: 'network-only' })
          }
        >
          Refresh
        </button>
      </div>
      <p role="status" className="my-2 text-sm text-neutral-11">
        {loading ? 'Loading…' : target ? `${targets.pagination.total} targets` : null}
      </p>
      {error ? (
        <FormError error={error} />
      ) : target ? (
        targets.data?.crawlerTargets ? (
          <Targets
            rows={targets.data.crawlerTargets.nodes}
            pagination={targets.pagination}
          />
        ) : (
          <Spinner />
        )
      ) : overview.data ? (
        <Overview {...overview.data} all={filters.all} allowRefetch={allowRefetch} />
      ) : (
        <Spinner />
      )}
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
        empty="No frontiers found"
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
}: CrawlerOverviewQuery & { all: string; allowRefetch: boolean }) {
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
      <div className="col-full-width overflow-x-auto">
        <table className="w-full whitespace-nowrap text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'target',
                'done',
                'due',
                'fetch errors',
                'process errors',
                'process',
                'last fetched',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {frontiers
              .filter((row) => all === '1' || !row.stable)
              .map((row) => (
                <Fragment key={row.target}>
                  <tr className="border-t border-neutral-4">
                    <th scope="row" className="px-2 py-1 font-mono font-normal">
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
                    </th>
                    <td className="px-2 py-1">{row.done || ''}</td>
                    <td className="px-2 py-1">{row.fetchDue || ''}</td>
                    <td className="px-2 py-1">
                      {row.fetchError || ''}
                      {!!row.fetchTransient &&
                        (row.fetchError
                          ? ` (+${row.fetchTransient} temp)`
                          : `${row.fetchTransient} temp`)}
                    </td>
                    <td className="px-2 py-1">{row.processError || ''}</td>
                    <td className="px-2 py-1">{row.processReady || ''}</td>
                    <td className="px-2 py-1">
                      {row.latest
                        ? numericFullFormatter.format(new Date(row.latest))
                        : '-'}
                    </td>
                  </tr>
                  {row.failures.length > 0 && (
                    <tr>
                      <td colSpan={7} className="px-2 pb-1">
                        <details>
                          <summary className="cursor-pointer">Failures</summary>
                          <table className="w-full whitespace-normal text-left">
                            <thead>
                              <tr>
                                {['count', 'failure', 'http', 'details'].map((label) => (
                                  <th key={label} scope="col" className="px-2 py-1">
                                    {label}
                                  </th>
                                ))}
                              </tr>
                            </thead>
                            <tbody>
                              {row.failures.map((failure) => (
                                <tr
                                  key={JSON.stringify([
                                    failure.failure,
                                    failure.httpStatus,
                                    failure.errorFingerprint,
                                  ])}
                                  className="border-t border-neutral-4 align-top"
                                >
                                  <td className="px-2 py-1">{failure.count}</td>
                                  <td className="px-2 py-1">{failure.failure}</td>
                                  <td className="px-2 py-1">{failure.httpStatus}</td>
                                  <td className="px-2 py-1 break-all">
                                    {failure.errorFingerprint}
                                  </td>
                                </tr>
                              ))}
                            </tbody>
                          </table>
                        </details>
                      </td>
                    </tr>
                  )}
                </Fragment>
              ))}
          </tbody>
          <tfoot>
            <tr className="border-t border-neutral-4">
              <th scope="row" className="px-2 py-1 font-normal">
                {frontiers.length} total
              </th>
              <td className="px-2 py-1">
                {frontiers.reduce((sum, row) => sum + (row.done ?? 0), 0) || ''}
              </td>
              <td className="px-2 py-1">
                {frontiers.reduce((sum, row) => sum + (row.fetchDue ?? 0), 0) || ''}
              </td>
              <td colSpan={4} />
            </tr>
          </tfoot>
        </table>
      </div>
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
