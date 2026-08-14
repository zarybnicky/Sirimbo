/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import { EventList } from '@/ui/lists/EventList';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Nadcházející akce',
  description:
    'Přehled nadcházejících akcí, soustředění, kempů, soutěží a dalších klubových událostí.',
  alternates: { canonical: '/akce' },
};

export default function EventsPage() {
  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <div className="col-feature min-h-[60vh] mt-16 mb-8">
        <EventList />
      </div>
    </Layout>
  );
}
