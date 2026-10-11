import { notFound } from 'next/navigation';
import { z } from 'zod';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationDocument } from '@/graphql/Federation';
import { Browser, type BrowserResult } from './Browser';

export const metadata = {
  title: 'Federation data',
  robots: { index: false, follow: false },
};

export default async function FederationPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string }>;
  searchParams: Promise<{ search?: string; date?: string; page?: string; view?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation } = await params;
  const { search = '', date = '', page, view = 'events' } = await searchParams;
  if (view !== 'events' && view !== 'people' && view !== 'competitors') notFound();
  if (view === 'events' && date && !z.iso.date().safeParse(date).success) notFound();
  let currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  const variables = {
    federation,
    events: view === 'events',
    people: view === 'people',
    competitors: view === 'competitors',
    eventCondition: {
      search: search.trim(),
      onDate: view === 'events' && date ? date : null,
    },
    personCondition: { search: search.trim() },
    competitorCondition: { search: search.trim() },
    offset: Math.min((currentPage - 1) * 50, 2_147_483_647),
  };
  let { federation: item } = await executeGraphql(FederationDocument, variables);
  if (!item) notFound();
  const total = item[view]?.totalCount ?? 0;
  const lastPage = Math.max(1, Math.ceil(total / 50));
  if (currentPage > lastPage) {
    currentPage = lastPage;
    ({ federation: item } = await executeGraphql(FederationDocument, {
      ...variables,
      offset: (currentPage - 1) * 50,
    }));
    if (!item) notFound();
  }
  const result: BrowserResult =
    view === 'events'
      ? { view, total, page: currentPage, rows: item.events?.nodes ?? [] }
      : { view, total, page: currentPage, rows: item[view]?.nodes ?? [] };

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={item.name}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: item.name },
        ]}
      />
      <Browser federation={federation} result={result} />
    </Layout>
  );
}
