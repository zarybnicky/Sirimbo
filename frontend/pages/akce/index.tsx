import * as React from 'react';
import { useAuth } from '@/ui/use-auth';
import { EventList } from '@/ui/lists/EventList';
import { NextSeo } from 'next-seo';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';

export default function EventsPage() {
  const auth = useAuth();
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Nadcházející akce" />
      {auth.user ? (
        <WithSidebar sidebar={<EventList />} />
      ) : (
        <div className="col-feature min-h-[60vh] mt-16 mb-8">
          <EventList />
        </div>
      )}
    </Layout>
  );
}
