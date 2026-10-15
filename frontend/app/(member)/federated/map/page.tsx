import type { Metadata } from 'next';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { competitionEventLocations } from '@/app/(standalone)/tools/tools.queries';
import EventMap from './EventMap';

export const dynamic = 'force-dynamic';

export const metadata: Metadata = {
  title: 'Competition map',
  robots: { index: false, follow: false },
};

export default async function Page() {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const year = new Date().getFullYear();
  const events = await runQuery(competitionEventLocations, {
    federation: 'csts',
    fromDate: `${year - 3}-12-31`,
    toDate: `${year}-12-31`,
  });

  return (
    <Layout requireUser>
      <PageHeader
        title="Competition map"
        breadcrumbs={[{ label: 'Federations', href: '/federated' }]}
      />
      <EventMap events={events} />
    </Layout>
  );
}
