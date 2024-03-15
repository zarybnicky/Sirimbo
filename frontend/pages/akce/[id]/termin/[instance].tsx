import { Layout } from '@/components/layout/Layout';
import { BasicEventInfo } from '@/ui/EventView';
import { InstanceAttendanceView } from '@/ui/InstanceAttendanceView';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatDefaultEventName } from '@/ui/format';
import { EventDocument } from '@app/graphql/Event';
import { EventList } from '@app/ui/EventList';
import { useAuth } from '@app/ui/use-auth';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
  instance: zRouterId,
});

function EventInstancePage() {
  const router = useTypedRouter(QueryParams);
  const { user } = useAuth();
  const { id } = router.query;
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      <WithSidebar sidebar={<EventList />}>
        {event && <TitleBar title={event?.name || formatDefaultEventName(event)} />}
        <div className={user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
          {data?.event && <BasicEventInfo event={data.event} />}
          <InstanceAttendanceView id={router.query.instance} />
        </div>
      </WithSidebar>
    </Layout>
  );
};

export default EventInstancePage;
