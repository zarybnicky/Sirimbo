import * as React from 'react';
import { EventDocument } from '@app/graphql/Event';
import { useAuth } from '@app/ui/use-auth';
import { EventList } from '@app/ui/EventList';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { AttendanceView } from '@/ui/AttendanceView';
import { BasicEventInfo } from '@/ui/EventView';
import { formatDefaultEventName } from '@/ui/format';
import { TitleBar } from '@/ui/TitleBar';

const Page = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
  const instance = fromSlugArray(router.query.instance);
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      <WithSidebar sidebar={<EventList />}>
        {event && <TitleBar title={event?.name || formatDefaultEventName(event)} />}
        <div className={user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
          {data?.event && <BasicEventInfo event={data.event} />}
          <AttendanceView id={instance} />
        </div>
      </WithSidebar>
    </Layout>
  );
};

export default Page;
