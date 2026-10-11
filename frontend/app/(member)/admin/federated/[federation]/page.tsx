import { notFound } from 'next/navigation';
import { z } from 'zod';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import {
  browseFederation,
  browseEventCount,
  browseEvents,
  browsePeopleCount,
  browsePeople,
  browseCompetitorsCount,
  browseSearchCompetitors,
} from '../federated.queries';
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
  const [item] = await runQuery(browseFederation, { federation });
  if (!item) notFound();
  const filters = { federation, search: search.trim(), date };
  const [count] =
    view === 'events'
      ? await runQuery(browseEventCount, filters)
      : view === 'people'
        ? await runQuery(browsePeopleCount, filters)
        : await runQuery(browseCompetitorsCount, filters);
  const total = count?.total ?? 0;
  const currentPage = Math.min(
    Math.max(1, Number.parseInt(page ?? '1', 10) || 1),
    Math.max(1, Math.ceil(total / 50)),
  );
  const query = { ...filters, offset: (currentPage - 1) * 50 };
  const result: BrowserResult =
    view === 'events'
      ? { view, total, page: currentPage, rows: await runQuery(browseEvents, query) }
      : {
          view,
          total,
          page: currentPage,
          rows:
            view === 'people'
              ? await runQuery(browsePeople, query)
              : await runQuery(browseSearchCompetitors, query),
        };

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
