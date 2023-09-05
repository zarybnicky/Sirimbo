import * as React from 'react';
import { EventDocument } from '@app/graphql/Event';
import { EventView } from '@app/ui/EventView';
import { useAuth } from '@app/ui/use-auth';
import { EventList } from '@app/ui/EventList';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
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
