'use client';

import { CampSchedule } from '@/calendar/CampSchedule';
import { CampLessonsTable } from '@/calendar/CampLessonsTable';
import { CampTrainersTable } from '@/calendar/CampTrainersTable';
import type { EventType } from '@/graphql';
import { EventWithAttendanceDocument } from '@/graphql/Event';
import { eventInstanceActions } from '@/lib/actions/eventInstance';
import { useActions } from '@/lib/actions';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { EventAttendance } from '@/ui/EventAttendance';
import { EventPayments } from '@/ui/EventPayments';
import { EventRegistrations } from '@/ui/EventRegistrations';
import { Layout } from '@/ui/Layout';
import { EventList } from '@/ui/lists/EventList';
import { RichTextView } from '@/ui/RichTextView';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatEventType, formatInstanceName } from '@/ui/format';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';
import type { ISharedEventInstanceResult } from './termin.queries';

export function EventPageClient({
  id,
  shared,
}: {
  id: string;
  shared: ISharedEventInstanceResult | null | undefined;
}) {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [{ data, fetching }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !/^\d{1,18}$/.test(id),
  });
  const instance = data?.eventInstance;
  const event = instance ?? shared;
  const description = event?.description;
  const actions = useActions(eventInstanceActions, instance);
  const title =
    (instance
      ? formatInstanceName(instance)
      : shared &&
        (shared.name?.trim() ||
          formatEventType(shared.type?.toUpperCase() as EventType | null))) || '';
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  React.useEffect(() => {
    if (title) document.title = title;
  }, [title]);

  const tabs = React.useMemo(() => {
    const tabs: {
      id: string;
      title: React.ReactNode;
      contents: () => React.ReactNode;
    }[] = [];
    if (!event) return tabs;

    if (description) {
      tabs.push({
        id: 'info',
        title: 'Informace',
        contents: () => <RichTextView value={description} />,
      });
    }

    if (instance) {
      const numRegistrations =
        instance.registrations.totalCount +
        instance.eventExternalRegistrationsByInstanceIdList.length;
      if (auth.user?.id && numRegistrations > 0) {
        tabs.push({
          id: 'registrations',
          title: `Přihlášky (${numRegistrations})`,
          contents: () => (
            <div className="col-popout">
              <EventRegistrations instance={instance} />
            </div>
          ),
        });
      }
    }

    const schedule =
      instance && auth.user?.id
        ? instance
        : shared?.hasTokenAccess || shared?.hasPublicDetails
          ? shared
          : null;
    if (schedule?.type?.toUpperCase() === 'CAMP') {
      tabs.push({
        id: 'schedule',
        title: 'Rozpis',
        contents: () => (
          <CampSchedule id={schedule.id} since={schedule.since} until={schedule.until} />
        ),
      });
    }

    if (instance?.type === 'CAMP') {
      tabs.push(
        {
          id: 'lessons',
          title: 'Lekce',
          contents: () => (
            <div className="col-full-width relative">
              <CampLessonsTable id={instance.id} />
            </div>
          ),
        },
        {
          id: 'trainers',
          title: 'Trenéři',
          contents: () => (
            <div className="col-full-width relative">
              <CampTrainersTable
                id={instance.id}
                since={instance.since}
                until={instance.until}
              />
            </div>
          ),
        },
      );
    }
    if (auth.isTrainerOrAdmin && instance) {
      tabs.push(
        {
          id: 'attendance',
          title: 'Docházka',
          contents: () => (
            <div className="col-popout">
              <EventAttendance id={instance.id} />
            </div>
          ),
        },
        {
          id: 'payments',
          title: 'Platby',
          contents: () => (
            <div className="col-popout">
              <EventPayments id={instance.id} />
            </div>
          ),
        },
      );
    }
    return tabs;
  }, [auth.isTrainerOrAdmin, auth.user?.id, description, event, instance, shared]);

  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <WithSidebar
        sidebar={<EventList />}
        className={auth.user ? 'p-4 lg:pb-8' : 'min-h-[60vh] mb-8'}
      >
        <div className="col-feature">
          {event && <PageHeader title={title} actions={actions} />}
          {event && <BasicEventInfo instance={event} />}
          {!authLoading && !fetching && !event && (
            <div className="my-12 rounded-md border border-neutral-5 bg-neutral-2 p-6 text-center">
              <h1 className="text-xl text-neutral-12">Termín nebyl nalezen</h1>
              <p className="mt-2 text-neutral-11">
                Odkaz není platný, byl zrušen nebo k termínu nemáte přístup.
              </p>
            </div>
          )}
        </div>
        <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      </WithSidebar>
    </Layout>
  );
}
