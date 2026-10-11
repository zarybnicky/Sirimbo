import { notFound } from 'next/navigation';
import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { crawlerResponseContent } from '../../../crawler.queries';

export const metadata = {
  title: 'Crawler response',
  robots: { index: false, follow: false },
};

export default async function ResponsePage({
  params,
}: {
  params: Promise<{ id: string; responseId: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const { id, responseId } = await params;
  if (!/^[0-9]{1,18}$/.test(id) || !/^[0-9]{1,18}$/.test(responseId)) notFound();
  const [response] = await runQuery(crawlerResponseContent, {
    frontierId: id,
    responseId,
  });
  if (!response) notFound();

  const frontierTitle = `${response.federation}:${response.kind}${response.key ? `:${response.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={`Response ${response.id}`}
        breadcrumbs={[
          { label: 'Crawler', href: '/admin/crawler' },
          {
            label: `${response.federation}:${response.kind}`,
            href: {
              pathname: '/admin/crawler',
              query: { federation: response.federation, kind: response.kind },
            },
          },
          { label: frontierTitle, href: `/admin/crawler/${id}` },
          { label: `Response ${response.id}` },
        ]}
      />
      <p className="break-all">{response.url}</p>
      <p>HTTP {response.http_status ?? '-'}</p>
      {response.error && (
        <pre className="whitespace-pre-wrap break-all">{response.error}</pre>
      )}
      <pre className="col-full-width overflow-x-auto text-sm">
        {response.content == null
          ? 'No cached content'
          : typeof response.content === 'string'
            ? response.content
            : JSON.stringify(response.content, null, 2)}
      </pre>
    </Layout>
  );
}
