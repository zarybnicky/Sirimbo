import type { Metadata } from 'next';
import { Fragment } from 'react';
import Link from 'next/link';
import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import {
  crawlerOverview,
  crawlerFailures,
  crawlerFrontiers,
  crawlerFrontierCount,
} from './crawler.queries';
import { numericFullFormatter } from '@/ui/format';
import { Filters } from './Filters';
import { Pagination } from '@/ui/Pagination';

export const metadata: Metadata = {
  title: 'Crawler',
  robots: { index: false, follow: false },
};

export default async function CrawlerPage({
  searchParams,
}: {
  searchParams: Promise<{
    federation?: string;
    kind?: string;
    all?: string;
    key?: string;
    page?: string;
  }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const { federation = '', kind = '', all, key = '', page } = await searchParams;

  if (federation && kind) {
    const filters = { federation, kind, key };
    const [count] = await runQuery(crawlerFrontierCount, filters);
    const total = count?.total ?? 0;
    const currentPage = Math.min(
      Math.max(1, Number.parseInt(page ?? '1', 10) || 1),
      Math.max(1, Math.ceil(total / 50)),
    );
    const rows = await runQuery(crawlerFrontiers, {
      ...filters,
      offset: (currentPage - 1) * 50,
    });
    const target = `${federation}:${kind}`;
    const query = { federation, kind, ...(key ? { key } : {}) };
    return (
      <Layout requireSystemAdmin>
        <PageHeader
          title={target}
          breadcrumbs={[{ label: 'Crawler', href: '/admin/crawler' }, { label: target }]}
        />
        <Filters />
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
                  <td className="px-2 py-1">{row.fetch_status}</td>
                  <td className="px-2 py-1">{row.process_status}</td>
                  <td className="px-2 py-1">{row.error_count || ''}</td>
                  <td className="px-2 py-1">
                    {row.last_fetched_at
                      ? numericFullFormatter.format(row.last_fetched_at)
                      : '-'}
                  </td>
                  <td className="px-2 py-1">
                    {numericFullFormatter.format(row.next_fetch_at)}
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
          total={total}
          limit={50}
          page={currentPage}
          href={(page) =>
            `/admin/crawler?${new URLSearchParams({ ...query, page: String(page) })}`
          }
        />
      </Layout>
    );
  }
  const params = { federation: federation || null, kind: kind || null };
  const [rows, failures] = await Promise.all([
    runQuery(crawlerOverview, {
      ...params,
      allowRefetch: process.env.CRAWLER_DISABLE_REFETCH !== 'true',
    }),
    runQuery(crawlerFailures, params),
  ]);
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
    <Layout requireSystemAdmin>
      <PageHeader title="Crawler" />
      <Filters />
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
    </Layout>
  );
}
