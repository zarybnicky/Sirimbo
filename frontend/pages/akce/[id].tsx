import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { EventDocument } from '@app/graphql/Event';
import { EventList } from '@app/ui/EventList';
import { EventView } from '@app/ui/EventView';
import { useAuth } from '@app/ui/use-auth';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

const Page = () => {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const { user } = useAuth();
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      <WithSidebar sidebar={<EventList />}>
        <div className={user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
          <EventView id={id} />
        </div>
      </WithSidebar>
    </Layout>
  );
};

export default Page;
