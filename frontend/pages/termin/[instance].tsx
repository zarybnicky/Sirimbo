import { Layout } from '@/ui/Layout';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { EventInstancePayments } from '@/ui/EventInstancePayments';
import { EventInstanceRegistrations } from '@/ui/EventInstanceRegistrations';
import { InstanceAttendanceView } from '@/ui/InstanceAttendanceView';
import { RichTextView } from '@/ui/RichTextView';
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
import { parseAsString, useQueryState } from 'nuqs';
import { TabMenu } from '@/ui/TabMenu';
import React from 'react';
import { useActions } from '@/lib/actions';
import { eventInstanceActions } from '@/lib/actions/eventInstance';

const QueryParams = z.object({
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
  const actions = useActions(eventInstanceActions, instance);
  const title = instance?.name || (instance ? formatDefaultInstanceName(instance) : '');
  const description = stripHtml(instance?.summary);
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  const tabs = React.useMemo(() => {
    const tabs: {
      id: string;
      title: React.ReactNode;
      contents: () => React.ReactNode;
    }[] = [];
    if (!instance) return tabs;

    if (instance.description) {
      tabs.push({
        id: 'info',
        title: 'Informace',
        contents: () => <RichTextView value={instance.description!} />,
      });
    }

    const numRegistrations =
      instance.registrations.totalCount +
      instance.eventExternalRegistrationsByInstanceIdList.length;
    if (auth.user?.id && numRegistrations > 0) {
      tabs.push({
        id: 'registrations',
        title: `Přihlášky (${numRegistrations})`,
        contents: () => <EventInstanceRegistrations instance={instance} />,
      });
    }

    if (instance.type === 'CAMP') {
      tabs.push({
        id: 'schedule',
        title: 'Rozpis',
        contents: () => <Calendar parentId={instance.id} initialDate={instance.since} />,
      });
    }

    if (auth.isTrainerOrAdmin) {
      tabs.push(
        {
          id: 'attendance',
          title: 'Docházka',
          contents: () => <InstanceAttendanceView id={instance.id} />,
        },
        {
          id: 'payments',
          title: 'Platby',
          contents: () => <EventInstancePayments instanceId={instance.id} />,
        },
      );
    }
    return tabs;
  }, [auth.isTrainerOrAdmin, auth.user?.id, instance]);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={title} description={description || undefined} />
      <WithSidebar sidebar={<EventList />}>
        <div
          className={
            auth.user ? 'col-feature p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'
          }
        >
          {instance && <PageHeader title={title} actions={actions} />}
          {instance && <BasicEventInfo instance={instance} />}
          <div className="max-w-full">
            <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
          </div>
        </div>
      </WithSidebar>
    </Layout>
  );
}

export default EventInstancePage;
