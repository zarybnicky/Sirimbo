import { EventList } from '@/ui/lists/EventList';
import { NextSeo } from 'next-seo';
import { Layout } from '@/ui/Layout';

export default function EventsPage() {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo
        title="Nadcházející akce"
        description="Přehled nadcházejících akcí, soustředění, kempů, soutěží a dalších klubových událostí."
      />
      <div className="col-feature min-h-[60vh] mt-16 mb-8">
        <EventList />
      </div>
    </Layout>
  );
}
