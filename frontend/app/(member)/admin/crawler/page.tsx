import type { Metadata } from 'next';
import { Fragment } from 'react';
import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { crawlerOverview, crawlerFailures } from './crawler.queries';
import { numericFullFormatter } from '@/ui/format';
import { Filters } from './Filters';

export const metadata: Metadata = {
  title: 'Crawler',
  robots: { index: false, follow: false },
};

export default async function CrawlerPage({
  searchParams,
}: {
  searchParams: Promise<{ federation?: string; kind?: string; all?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const { federation = '', kind = '', all } = await searchParams;
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
      <h1 className="text-2xl font-bold">Crawler</h1>
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
                      {row.target}
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
                    <td className="px-2 py-1 font-mono">{row.keys}</td>
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
                                    {failure.samples}
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
          <h2>Done</h2>
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
                  .map((row) =>
                    row.total === 1 ? row.kind : `${row.kind} (${row.total})`,
                  )
                  .join(', ')}
              </p>
            ))}
        </section>
      )}
    </Layout>
  );
}
