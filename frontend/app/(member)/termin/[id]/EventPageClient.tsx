'use client';

import { CampSchedule } from '@/calendar/CampSchedule';
import { CampLessonsTable } from '@/calendar/CampLessonsTable';
import { CampTrainersTable } from '@/calendar/CampTrainersTable';
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
  initialEvent: NonNullable<EventWithAttendanceQuery['event']>;
  hasShareToken: boolean;
}) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !/^\d{1,18}$/.test(id),
  });
  const event = data?.event ?? initialEvent;
  const actions = useActions(eventInstanceActions, event);
  const primaryAction = actions.some((action) => action.id === 'eventInstance.edit')
    ? 'eventInstance.edit'
    : 'eventInstance.registrations';
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  const numRegistrations = event.registrationInfo?.registrations ?? 0;
  const hasPayments = [event, ...event.childEventInstancesList].some(
    (e) => e.paymentsList.length > 0,
  );

  return (
    <Layout hideTopMenuIfLoggedIn>
      <div className="col-feature">
        <PageHeader
          title={formatEventName(event) || ''}
          subtitle={formatEventType(event.type)}
          actions={actions}
          primary={primaryAction}
        />
      </div>
      <TabMenu
        className="col-feature"
        selected={tab === 'attendance' ? 'registrations' : tab}
        onSelect={setTab}
      >
        {event.type === 'CAMP' &&
          (auth.isLoggedIn || event.hasPublicDetails || hasShareToken) && (
            <Tab id="schedule" title="Rozpis">
              <CampSchedule id={event.id} since={event.since} until={event.until} />
            </Tab>
          )}
        <Tab id="info" title="Info">
          <BasicEventInfo instance={event} />
        </Tab>
        {(auth.isTrainer || (auth.isLoggedIn && numRegistrations > 0)) && (
          <Tab id="registrations" title={`Přihlášky (${numRegistrations})`}>
            <div className="col-popout">
              <EventRegistrations instance={event} />
            </div>
          </Tab>
        )}
        {event.type === 'CAMP' && event.childEventInstancesList.length > 0 && (
          <Tab id="lessons" title="Lekce" requireTrainer>
            <div className="col-full-width relative">
              <CampLessonsTable id={event.id} />
            </div>
          </Tab>
        )}
        {event.type === 'CAMP' && event.childEventInstancesList.length > 0 && (
          <Tab id="trainers" title="Trenéři" requireTrainer>
            <div className="col-full-width relative">
              <CampTrainersTable id={event.id} />
            </div>
          </Tab>
        )}
        {hasPayments && (
          <Tab id="payments" title="Platby" requireTrainer>
            <div className="col-popout">
              <EventPayments id={event.id} />
            </div>
          </Tab>
        )}
      </TabMenu>
    </Layout>
  );
}
