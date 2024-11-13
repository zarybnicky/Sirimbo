import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { EventDocument } from '@/graphql/Event';
import { EventList } from '@/ui/lists/EventList';
import { EventView } from '@/ui/EventView';
import { useAuth } from '@/ui/use-auth';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function EventPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const auth = useAuth();
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      <WithSidebar sidebar={<EventList />}>
        <div className={auth.user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
          <EventView id={id} />
        </div>
      </WithSidebar>
    </Layout>
  );
};
