import { Layout } from '@/ui/Layout';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { InstanceAttendanceView } from '@/ui/InstanceAttendanceView';
import { stripHtml } from '@/lib/seo';
import { PageHeader } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatDefaultInstanceName } from '@/ui/format';
import { EventInstanceWithAttendanceDocument } from '@/graphql/Event';
import { EventList } from '@/ui/lists/EventList';
import { useAuth } from '@/ui/use-auth';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { Calendar } from '@/calendar/Calendar';

const QueryParams = z.object({
  id: zRouterId,
  instance: zRouterId,
});

function EventInstancePage() {
  const router = useTypedRouter(QueryParams);
  const auth = useAuth();
  const { instance: instanceId } = router.query;
  const [{ data }] = useQuery({
    query: EventInstanceWithAttendanceDocument,
    variables: { id: instanceId },
    pause: !instanceId,
  });
  const instance = data?.eventInstance;
  const title = instance?.name || (instance ? formatDefaultInstanceName(instance) : '');
  const description = stripHtml(instance?.summary);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={title} description={description || undefined} />
      <WithSidebar sidebar={<EventList />}>
        <div
          className={
            auth.user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'
          }
        >
          {instance && <PageHeader title={title} />}
          {instance && <BasicEventInfo instance={instance} />}
          {instance?.type === 'CAMP' && (
            <Calendar parentId={instance.id} initialDate={instance.since} />
          )}
          <InstanceAttendanceView id={router.query.instance} />
        </div>
      </WithSidebar>
    </Layout>
  );
}

export default EventInstancePage;
