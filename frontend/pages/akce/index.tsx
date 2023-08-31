import * as React from 'react';
import { useAuth } from '@app/ui/use-auth';
import { EventMemberList } from '@app/ui/EventMemberList';
import { NextSeo } from 'next-seo';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  const { user } = useAuth();
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Nadcházející akce" />
      <div className={user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
        <EventMemberList />
      </div>
    </Layout>
  );
}

export default Page;
