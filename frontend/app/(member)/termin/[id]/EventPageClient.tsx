'use client';

import { CampSchedule } from '@/calendar/CampSchedule';
import { CampLessonsTable } from '@/calendar/CampLessonsTable';
import { CampTrainersTable } from '@/calendar/CampTrainersTable';
import type { EventType } from '@/graphql';
import {
  EventWithAttendanceDocument,
  type EventWithAttendanceQuery,
} from '@/graphql/Event';
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
import { useAuth } from '@/lib/auth';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';

export function EventPageClient({
  id,
  initialEvent,
  hasShareToken,
}: {
  id: string;
  initialEvent: EventWithAttendanceQuery['event'];
  hasShareToken: boolean;
}) {
  const auth = useAuth();
  const [{ data, fetching }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !/^\d{1,18}$/.test(id),
  });
  const instance = data ? data.event : initialEvent;
  const actions = useActions(eventInstanceActions, instance);
  const primaryAction = actions.some((action) => action.id === 'eventInstance.edit')
    ? 'eventInstance.edit'
    : 'eventInstance.registrations';
  const title = instance ? formatEventName(instance) || '' : '';
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

    const schedule =
      auth.user?.id || instance.hasPublicDetails || hasShareToken ? instance : null;
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
      contents: () => <BasicEventInfo instance={instance} />,
    });


    const numRegistrations = instance.registrationInfo?.registrations ?? 0;
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

    if (instance?.type === 'CAMP' && auth.isTrainerOrAdmin) {
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
              <CampTrainersTable id={instance.id} />
            </div>
          ),
        },
      );
    }
    if (auth.isTrainerOrAdmin) {
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
  }, [auth.isTrainerOrAdmin, auth.user?.id, hasShareToken, instance]);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <div className="col-feature">
        {instance && (
          <PageHeader
            title={title}
            subtitle={formatEventType(instance.type?.toUpperCase() as EventType | null)}
            actions={actions}
            primary={primaryAction}
          />
        )}
        {!fetching && !instance && (
          <div className="my-12 rounded-md border border-neutral-5 bg-neutral-2 p-6 text-center">
            <h1 className="text-xl text-neutral-12">Událost nenalezena</h1>
            <p className="mt-2 text-neutral-11">
              Odkaz není platný, nebo k události nemáte přístup.
            </p>
          </div>
        )}
      </div>
      <TabMenu
        className="col-feature"
        selected={variant}
        onSelect={setVariant}
        options={tabs}
      />
    </Layout>
  );
}
