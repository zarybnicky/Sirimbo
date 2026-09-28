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
import { EventPayments } from '@/ui/EventPayments';
import { EventRegistrations } from '@/ui/EventRegistrations';
import { Layout } from '@/ui/Layout';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { formatEventName, formatEventType } from '@/ui/format';
import { useAuth } from '@/lib/auth';
import { parseAsString, useQueryState } from 'nuqs';
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
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  const showSchedule =
    instance?.type?.toUpperCase() === 'CAMP' &&
    (auth.isLoggedIn || instance.hasPublicDetails || hasShareToken);
  const numRegistrations = instance?.registrationInfo?.registrations ?? 0;

  return (
    <Layout hideTopMenuIfLoggedIn>
      <div className="col-feature">
        {instance && (
          <PageHeader
            title={instance ? formatEventName(instance) || '' : ''}
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
        selected={variant === 'attendance' ? 'registrations' : variant}
        onSelect={setVariant}
      >
        {instance && (
          <>
            {showSchedule && (
              <Tab id="schedule" title="Rozpis">
                <CampSchedule
                  id={instance.id}
                  since={instance.since}
                  until={instance.until}
                />
              </Tab>
            )}
            <Tab id="info" title="Info">
              <BasicEventInfo instance={instance} />
            </Tab>
            {(auth.isTrainer || (auth.isLoggedIn && numRegistrations > 0)) && (
              <Tab id="registrations" title={`Přihlášky (${numRegistrations})`}>
                <div className="col-popout">
                  <EventRegistrations instance={instance} />
                </div>
              </Tab>
            )}
            {instance.type === 'CAMP' && (
              <>
                <Tab id="lessons" title="Lekce" requireTrainer>
                  <div className="col-full-width relative">
                    <CampLessonsTable id={instance.id} />
                  </div>
                </Tab>
                <Tab id="trainers" title="Trenéři" requireTrainer>
                  <div className="col-full-width relative">
                    <CampTrainersTable id={instance.id} />
                  </div>
                </Tab>
              </>
            )}
            <Tab id="payments" title="Platby" requireTrainer>
              <div className="col-popout">
                <EventPayments id={instance.id} />
              </div>
            </Tab>
          </>
        )}
      </TabMenu>
    </Layout>
  );
}
