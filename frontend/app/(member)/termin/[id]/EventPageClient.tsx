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
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { formatEventType, formatEventName } from '@/ui/format';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';
import type { ISharedEventResult } from './termin.queries';

export function EventPageClient({
  id,
  shared,
}: {
  id: string;
  shared: ISharedEventResult | null | undefined;
}) {
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [{ data, fetching }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !/^\d{1,18}$/.test(id),
  });
  const instance = data?.event;
  const event = instance ?? shared;
  const actions = useActions(eventInstanceActions, instance);
  const primaryAction = actions.some((action) => action.id === 'eventInstance.edit')
    ? 'eventInstance.edit'
    : 'eventInstance.registrations';
  const title =
    (instance
      ? formatEventName(instance)
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

    tabs.push({
      id: 'info',
      title: 'Info',
      contents: () => <BasicEventInfo instance={event} />,
    });

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

    if (instance?.type === 'CAMP') {
      tabs.push(
        {
          id: 'lessons',
          title: 'Lekce',
          contents: () => (
            <div className="col-feature relative">
              <CampLessonsTable id={instance.id} />
            </div>
          ),
        },
        {
          id: 'trainers',
          title: 'Trenéři',
          contents: () => (
            <div className="col-feature relative">
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
  }, [auth.isTrainerOrAdmin, auth.user?.id, event, instance, shared]);

  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <div className="col-feature">
        {event && (
          <PageHeader
            title={title}
            subtitle={formatEventType(event.type?.toUpperCase() as EventType | null)}
            actions={actions}
            primary={primaryAction}
          />
        )}
        {!authLoading && !fetching && !event && (
          <div className="my-12 rounded-md border border-neutral-5 bg-neutral-2 p-6 text-center">
            <h1 className="text-xl text-neutral-12">Událost nenalezena</h1>
            <p className="mt-2 text-neutral-11">
              Odkaz není platný, nebo k události nemáte přístup.
            </p>
          </div>
        )}
      </div>
      <TabMenu className="col-feature" selected={variant} onSelect={setVariant} options={tabs} />
    </Layout>
  );
}
