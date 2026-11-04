import { DefinitionList } from '@/ui/DefinitionList';
import { notFound } from 'next/navigation';
import { executeGraphql } from '@/lib/server/graphql';
import { CrawlerResponseDocument } from '@/graphql/Crawler';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';

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

  const { id: identifier, responseId } = await params;
  if (!/^[0-9]{1,18}$/.test(responseId)) notFound();
  const { crawlerResponse: response } = await executeGraphql(CrawlerResponseDocument, {
    id: responseId,
  });
  if (!response?.frontier) notFound();
  const { frontier } = response;
  if (
    decodeURIComponent(identifier) !==
    `${frontier.federation}:${frontier.kind}:${frontier.key}`
  )
    notFound();
  const rawContent = response.crawlerResponseCache?.content;
  let content = rawContent;
  if (typeof content === 'string') {
    try {
      content = JSON.parse(content);
    } catch {}
  }

  const frontierTitle = `${frontier.federation}:${frontier.kind}${frontier.key ? `:${frontier.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={`Response ${response.id}`}
        breadcrumbLabel="Response"
        breadcrumbs={[
          { label: 'Crawler', href: '/admin/crawler' },
          {
            label: `${frontier.federation}:${frontier.kind}`,
            href: `/admin/crawler/${frontier.federation}:${frontier.kind}`,
          },
          {
            label: frontierTitle,
            href: `/admin/crawler/${frontier.federation}:${frontier.kind}:${encodeURIComponent(frontier.key)}`,
          },
        ]}
      />
      <DefinitionList
        items={{
          url: response.url,
          http: response.httpStatus ?? '-',
        }}
      />
      {response.error && (
        <pre className="whitespace-pre-wrap break-all">{response.error}</pre>
      )}
      <pre className="col-feature overflow-x-auto text-sm">
        {!rawContent
          ? 'No cached content'
          : typeof content === 'string'
            ? content
            : JSON.stringify(content, null, 2)}
      </pre>
    </Layout>
  );
}
