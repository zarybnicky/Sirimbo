import { notFound } from 'next/navigation';
import { executeGraphql } from '@/lib/server/graphql';
import { CrawlerTargetDocument } from '@/graphql/Crawler';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { Responses } from './Responses';
import { numericFullFormatter } from '@/ui/format';

export const metadata = {
  title: 'Crawler frontier',
  robots: { index: false, follow: false },
};

export default async function FrontierPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const { id } = await params;
  if (!/^[0-9]{1,18}$/.test(id)) notFound();
  const { crawlerTarget: frontier } = await executeGraphql(CrawlerTargetDocument, { id });
  if (!frontier) notFound();

  const title = `${frontier.federation}:${frontier.kind}${frontier.key ? `:${frontier.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={title}
        breadcrumbLabel="Frontier"
        breadcrumbs={[
          { label: 'Crawler', href: '/admin/crawler' },
          {
            label: `${frontier.federation}:${frontier.kind}`,
            href: {
              pathname: '/admin/crawler',
              query: { federation: frontier.federation, kind: frontier.kind },
            },
          },
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
      <Responses id={id} />
    </Layout>
  );
}
