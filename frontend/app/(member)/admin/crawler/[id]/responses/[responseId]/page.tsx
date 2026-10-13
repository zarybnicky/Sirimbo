import { notFound } from 'next/navigation';
import { executeGraphql } from '@/lib/server/graphql';
import { CrawlerResponseDocument } from '@/graphql/Crawler';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';

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
  const { crawlerResponse: response } = await executeGraphql(
    CrawlerResponseDocument,
    { id: responseId },
  );
  if (!response || response.frontierId !== id || !response.frontier) notFound();
  const { frontier } = response;
  const rawContent = response.crawlerResponseCache?.content;
  const content = typeof rawContent === 'string' ? JSON.parse(rawContent) : rawContent;

  const frontierTitle = `${frontier.federation}:${frontier.kind}${frontier.key ? `:${frontier.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={`Response ${response.id}`}
        breadcrumbs={[
          { label: 'Crawler', href: '/admin/crawler' },
          {
            label: `${frontier.federation}:${frontier.kind}`,
            href: {
              pathname: '/admin/crawler',
              query: { federation: frontier.federation, kind: frontier.kind },
            },
          },
          { label: frontierTitle, href: `/admin/crawler/${id}` },
          { label: `Response ${response.id}` },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>url</dt>
        <dd>{response.url}</dd>
        <dt>http</dt>
        <dd>{response.httpStatus ?? '-'}</dd>
      </dl>
      {response.error && (
        <pre className="whitespace-pre-wrap break-all">{response.error}</pre>
      )}
      <pre className="col-feature overflow-x-auto text-sm">
        {content == null
          ? 'No cached content'
          : typeof content === 'string'
            ? content
            : JSON.stringify(content, null, 2)}
      </pre>
    </Layout>
  );
}
