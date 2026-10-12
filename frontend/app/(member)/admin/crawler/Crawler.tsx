'use client';

import { Fragment, useEffect, useState } from 'react';
import Link from 'next/link';
import { parseAsInteger, parseAsString, useQueryState, useQueryStates } from 'nuqs';
import { useQuery } from 'urql';
import { useAsync } from 'react-async-hook';
import { CrawlerTargetsDocument } from '@/graphql/Crawler';
import { TextField } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { buttonCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { numericFullFormatter } from '@/ui/format';
import { getOverview } from './actions';

export function Crawler() {
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
  const [refresh, setRefresh] = useState(0);
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
          onClick={() => setRefresh((value) => value + 1)}
        >
          Refresh
        </button>
      </div>
      {target ? (
        <Targets
          key={refresh}
          federation={search.federation}
          kind={search.kind}
          search={search.key}
          pending={pending}
        />
      ) : (
        <Overview
          key={refresh}
          federation={search.federation}
          kind={search.kind}
          all={filters.all}
        />
      )}
    </>
  );
}

function Targets({
  federation,
  kind,
  search,
  pending,
}: {
  federation: string;
  kind: string;
  search: string;
  pending: boolean;
}) {
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: CrawlerTargetsDocument,
    pause: pending,
    variables: {
      condition: { federation, kind, keyContains: search },
      offset: Math.min(Math.max(0, currentPage - 1) * 50, 2_147_483_647),
    },
  });
  const total = data?.crawlerTargets?.totalCount;
  useEffect(() => {
    if (pending || fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 50)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, pending, fetching, error, setPage]);
  if (error) return <FormError error={error} />;
  if (!data?.crawlerTargets) return <Spinner />;
  const rows = data.crawlerTargets.nodes;

  return (
    <>
      <p role="status" className="my-2 text-sm text-neutral-11">
        {pending || fetching ? 'Loading…' : `${total} targets`}
      </p>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full whitespace-nowrap text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'key',
                'fetch status',
                'process status',
                'error count',
                'last fetched',
                'next fetch',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-mono font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`/admin/crawler/${row.id}`}
                    prefetch={false}
                  >
                    {row.key || '(root)'}
                  </Link>
                </th>
                <td className="px-2 py-1">{row.fetchStatus.toLowerCase()}</td>
                <td className="px-2 py-1">{row.processStatus.toLowerCase()}</td>
                <td className="px-2 py-1">{row.errorCount || ''}</td>
                <td className="px-2 py-1">
                  {row.lastFetchedAt
                    ? numericFullFormatter.format(new Date(row.lastFetchedAt))
                    : '-'}
                </td>
                <td className="px-2 py-1">
                  {numericFullFormatter.format(new Date(row.nextFetchAt))}
                </td>
              </tr>
            ))}
            {rows.length === 0 && (
              <tr>
                <td colSpan={6} className="px-2 py-1">
                  No frontiers found
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={data.crawlerTargets.totalCount}
        limit={50}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}

function Overview({
  federation,
  kind,
  all,
}: {
  federation: string;
  kind: string;
  all: string;
}) {
  const { result, loading, error } = useAsync(getOverview, [federation, kind]);
  if (error) return <FormError error={error} />;
  if (!result) return <Spinner />;
  const { rows, failures } = result;
  const failuresByTarget = Object.groupBy(
    failures,
    (row): string => `${row.federation}:${row.kind}`,
  );
  const frontiers = rows.map((row) => {
    const target = `${row.federation}:${row.kind}`;
    const failures = failuresByTarget[target] ?? [];
    return {
      ...row,
      target,
      failures,
      stable:
        row.total === row.done &&
        !row.fetch_due &&
        !row.fetch_error &&
        !row.fetch_transient &&
        !row.process_error &&
        failures.length === 0,
    };
  });
  const done = Object.groupBy(
    frontiers.filter((row) => row.stable),
    (row) => row.federation,
  );

  return (
    <>
      <p role="status" className="my-2 text-sm text-neutral-11">
        {loading ? 'Loading…' : null}
      </p>
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
                'keys',
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
                    <td className="px-2 py-1">{row.fetch_due || ''}</td>
                    <td className="px-2 py-1">
                      {row.fetch_error || ''}
                      {!!row.fetch_transient &&
                        (row.fetch_error
                          ? ` (+${row.fetch_transient} temp)`
                          : `${row.fetch_transient} temp`)}
                    </td>
                    <td className="px-2 py-1">{row.process_error || ''}</td>
                    <td className="px-2 py-1">{row.process_ready || ''}</td>
                    <td className="px-2 py-1 font-mono">
                      {row.keys?.map((key, index) => (
                        <Fragment key={row.ids?.[index]}>
                          {index > 0 && ', '}
                          <Link
                            className="text-accent-11 underline"
                            href={`/admin/crawler/${row.ids?.[index]}`}
                            prefetch={false}
                          >
                            {key || '(root)'}
                          </Link>
                        </Fragment>
                      ))}
                      {(row.total ?? 0) > 3 && ', ...'}
                    </td>
                    <td className="px-2 py-1">
                      {row.latest ? numericFullFormatter.format(row.latest) : '-'}
                    </td>
                  </tr>
                  {row.failures.length > 0 && (
                    <tr>
                      <td colSpan={8} className="px-2 pb-1">
                        <details>
                          <summary className="cursor-pointer">Failures</summary>
                          <table className="w-full whitespace-normal text-left">
                            <thead>
                              <tr>
                                {['count', 'failure', 'http', 'samples', 'details'].map(
                                  (label) => (
                                    <th key={label} scope="col" className="px-2 py-1">
                                      {label}
                                    </th>
                                  ),
                                )}
                              </tr>
                            </thead>
                            <tbody>
                              {row.failures.map((failure) => (
                                <tr
                                  key={JSON.stringify([
                                    failure.failure,
                                    failure.http_status,
                                    failure.error_fingerprint,
                                  ])}
                                  className="border-t border-neutral-4 align-top"
                                >
                                  <td className="px-2 py-1">{failure.count}</td>
                                  <td className="px-2 py-1">{failure.failure}</td>
                                  <td className="px-2 py-1">{failure.http_status}</td>
                                  <td className="px-2 py-1 font-mono break-all">
                                    {failure.samples?.map((key, index) => (
                                      <Fragment key={failure.sample_ids?.[index]}>
                                        {index > 0 && ', '}
                                        <Link
                                          className="text-accent-11 underline"
                                          href={`/admin/crawler/${failure.sample_ids?.[index]}`}
                                          prefetch={false}
                                        >
                                          {key || '(root)'}
                                        </Link>
                                      </Fragment>
                                    ))}
                                  </td>
                                  <td className="px-2 py-1 break-all">
                                    {failure.error_fingerprint}
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
                {frontiers.reduce((sum, row) => sum + (row.fetch_due ?? 0), 0) || ''}
              </td>
              <td colSpan={5} />
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
                      (b.total ?? 0) - (a.total ?? 0) || a.kind.localeCompare(b.kind),
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
