import * as React from 'react';
import { useAuth } from '@app/ui/use-auth';
import { EventList } from '@app/ui/EventList';
import { NextSeo } from 'next-seo';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => {
  const { user } = useAuth();
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Nadcházející akce" />
      {user ? (
        <WithSidebar sidebar={<EventList />} />
      ) : (
        <div className="col-feature min-h-[60vh] mt-16 mb-8">
          <EventList />
        </div>
      )}
    </Layout>
  );
}

export default Page;
