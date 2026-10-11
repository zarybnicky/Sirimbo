import Link from 'next/link';
import { notFound } from 'next/navigation';
import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericFullFormatter } from '@/ui/format';
import {
  crawlerFrontier,
  crawlerResponses,
  crawlerResponseCount,
} from '../crawler.queries';

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
  const [frontier] = await runQuery(crawlerFrontier, { id });
  if (!frontier) notFound();
  const [count] = await runQuery(crawlerResponseCount, { id });
  const total = count?.total ?? 0;
  const currentPage = Math.min(
    Math.max(1, Number.parseInt(page ?? '1', 10) || 1),
    Math.max(1, Math.ceil(total / 50)),
  );
  const responses = await runQuery(crawlerResponses, {
    id,
    offset: (currentPage - 1) * 50,
  });

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
        <dd>{frontier.fetch_status}</dd>
        <dt>process status</dt>
        <dd>{frontier.process_status}</dd>
        <dt>error count</dt>
        <dd>{frontier.error_count}</dd>
        {(
          [
            ['discovered', frontier.discovered_at],
            ['last fetched', frontier.last_fetched_at],
            ['next fetch', frontier.next_fetch_at],
            ['last process error', frontier.last_process_error_at],
          ] as const
        ).map(([label, date]) => (
          <div key={label}>
            <dt>{label}</dt>
            <dd>{date ? numericFullFormatter.format(date) : '-'}</dd>
          </div>
        ))}
      </dl>
      {frontier.last_process_error && (
        <pre className="whitespace-pre-wrap break-all text-sm">
          {frontier.last_process_error}
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
                  {response.has_content ? (
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
                  {response.id === frontier.last_response_id && <div>latest</div>}
                  {response.id === frontier.last_successful_response_id && (
                    <div>last successful</div>
                  )}
                </td>
                <td className="px-2 py-1">{response.http_status ?? '-'}</td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.valid_from ? (
                    <>
                      <div>[{numericFullFormatter.format(response.valid_from)},</div>
                      <div>
                        {response.valid_until
                          ? numericFullFormatter.format(response.valid_until)
                          : '∞'}
                        )
                      </div>
                    </>
                  ) : (
                    <div>
                      {numericFullFormatter.format(response.fetched_at)} (fetched)
                    </div>
                  )}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.last_observed_at
                    ? numericFullFormatter.format(response.last_observed_at)
                    : '-'}
                </td>
                <td className="px-2 py-1">{response.observation_count}</td>
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
