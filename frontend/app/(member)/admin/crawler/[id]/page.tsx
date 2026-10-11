import Link from 'next/link';
import { notFound } from 'next/navigation';
import { executeGraphql } from '@/lib/server/graphql';
import { CrawlerTargetDocument } from '@/graphql/Crawler';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericFullFormatter } from '@/ui/format';

export const metadata = {
  title: 'Crawler frontier',
  robots: { index: false, follow: false },
};

export default async function FrontierPage({
  params,
  searchParams,
}: {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const { id } = await params;
  const { page } = await searchParams;
  if (!/^[0-9]{1,18}$/.test(id)) notFound();
  let currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  let { crawlerTarget: frontier } = await executeGraphql(CrawlerTargetDocument, {
    id,
    offset: (currentPage - 1) * 50,
  });
  if (!frontier) notFound();
  const total = frontier.responses.totalCount;
  const lastPage = Math.max(1, Math.ceil(total / 50));
  if (currentPage > lastPage) {
    currentPage = lastPage;
    ({ crawlerTarget: frontier } = await executeGraphql(CrawlerTargetDocument, {
      id,
      offset: (currentPage - 1) * 50,
    }));
    if (!frontier) notFound();
  }
  const responses = frontier.responses.nodes;

  const title = `${frontier.federation}:${frontier.kind}${frontier.key ? `:${frontier.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={title}
        breadcrumbs={[
          { label: 'Crawler', href: '/admin/crawler' },
          {
            label: `${frontier.federation}:${frontier.kind}`,
            href: {
              pathname: '/admin/crawler',
              query: { federation: frontier.federation, kind: frontier.kind },
            },
          },
          { label: title },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>id</dt>
        <dd>{frontier.id}</dd>
        <dt>fetch status</dt>
        <dd>{frontier.fetchStatus.toLowerCase()}</dd>
        <dt>process status</dt>
        <dd>{frontier.processStatus.toLowerCase()}</dd>
        <dt>error count</dt>
        <dd>{frontier.errorCount}</dd>
        {(
          [
            ['discovered', frontier.discoveredAt],
            ['last fetched', frontier.lastFetchedAt],
            ['next fetch', frontier.nextFetchAt],
            ['last process error', frontier.lastProcessErrorAt],
          ] as const
        ).map(([label, date]) => (
          <div key={label}>
            <dt>{label}</dt>
            <dd>{date ? numericFullFormatter.format(new Date(date)) : '-'}</dd>
          </div>
        ))}
      </dl>
      {frontier.lastProcessError && (
        <pre className="whitespace-pre-wrap break-all text-sm">
          {frontier.lastProcessError}
        </pre>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Responses</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'response',
                'http',
                'sys_period',
                'last observed',
                'observations',
                'url / error',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1 whitespace-nowrap">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {responses.map((response) => (
              <tr key={response.id} className="border-t border-neutral-4 align-top">
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.contentHash ? (
                    <Link
                      className="text-accent-11 underline"
                      href={`/admin/crawler/${id}/responses/${response.id}`}
                      prefetch={false}
                    >
                      {response.id}
                    </Link>
                  ) : (
                    response.id
                  )}
                  {response.id === frontier.lastResponseId && <div>latest</div>}
                  {response.id === frontier.lastSuccessfulResponseId && (
                    <div>last successful</div>
                  )}
                </td>
                <td className="px-2 py-1">{response.httpStatus ?? '-'}</td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.sysPeriod?.start?.value ? (
                    <>
                      <div>
                        [
                        {numericFullFormatter.format(
                          new Date(response.sysPeriod?.start?.value),
                        )}
                        ,
                      </div>
                      <div>
                        {response.sysPeriod?.end?.value
                          ? numericFullFormatter.format(
                              new Date(response.sysPeriod?.end?.value),
                            )
                          : '∞'}
                        )
                      </div>
                    </>
                  ) : (
                    <div>
                      {numericFullFormatter.format(new Date(response.fetchedAt))}{' '}
                      (fetched)
                    </div>
                  )}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.lastObservedAt
                    ? numericFullFormatter.format(new Date(response.lastObservedAt))
                    : '-'}
                </td>
                <td className="px-2 py-1">{response.observationCount}</td>
                <td className="min-w-64 px-2 py-1 break-all">
                  <div>{response.url}</div>
                  {response.error && (
                    <details>
                      <summary className="cursor-pointer">Error</summary>
                      <pre className="max-h-72 overflow-auto whitespace-pre-wrap">
                        {response.error}
                      </pre>
                    </details>
                  )}
                </td>
              </tr>
            ))}
            {responses.length === 0 && (
              <tr>
                <td colSpan={6} className="px-2 py-1">
                  No responses
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
        href={(page) => `/admin/crawler/${id}?page=${page}`}
      />
    </Layout>
  );
}
