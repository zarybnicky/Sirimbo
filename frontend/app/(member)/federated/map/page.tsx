import type { Metadata } from 'next';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import EventMap from './EventMap';

export const metadata: Metadata = {
  title: 'Competition map',
  robots: { index: false, follow: false },
};

export default function Page() {
  return (
    <Layout requireUser>
      <PageHeader
        title="Competition map"
        breadcrumbs={[{ label: 'Federations', href: '/federated' }]}
      />
      <EventMap federation="csts" />
    </Layout>
  );
}
