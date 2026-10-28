'use client';

import { CampSchedule } from '@/calendar/CampSchedule';
import { CampLessonsTable } from '@/calendar/CampLessonsTable';
import { CampTrainersTable } from '@/calendar/CampTrainersTable';
import {
  EventWithAttendanceDocument,
  UpdateEventDocument,
  type EventWithAttendanceQuery,
} from '@/graphql/Event';
import { eventActions } from '@/lib/actions/event';
import { useActions } from '@/lib/actions';
import { BasicEventInfo } from '@/ui/BasicEventInfo';
import { EventPayments } from '@/ui/EventPayments';
import { EventRegistrations } from '@/ui/EventRegistrations';
import { Layout } from '@/ui/Layout';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/PageHeader';
import { Dialog, DialogContent } from '@/ui/dialog';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { formatEventName, formatEventType } from '@/ui/format';
import { useAuth } from '@/lib/auth';
import { parseAsString, useQueryState } from 'nuqs';
import { useMutation, useQuery } from 'urql';
import React from 'react';
import { ImageEditorDialog } from '@/ui/ImageEditorDialog';

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
  const [editingImage, setEditingImage] = React.useState(false);
  const [, updateImage] = useMutation(UpdateEventDocument);
  const [{ data }] = useQuery({
    query: EventWithAttendanceDocument,
    variables: { id },
    pause: !/^\d{1,18}$/.test(id),
  });
  const event = data?.event ?? initialEvent;
  const actions = useActions(eventActions, event);
  const primaryAction = actions.some((x) => x.id === 'event.edit')
    ? 'event.edit'
    : 'event.registrations';
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [signup, setSignup] = useQueryState('signup');

  const numRegistrations = event.registrationInfo?.registrations ?? 0;
  const hasPayments = [event, ...event.childEventsList].some(
    (e) => e.paymentsList.length > 0,
  );

  return (
    <Layout hideTopMenuIfLoggedIn>
      {editingImage && (
        <ImageEditorDialog
          file={event.coverImage}
          onClose={() => setEditingImage(false)}
          onSave={async (coverImageId) => {
            const result = await updateImage({ id, patch: { coverImageId } });
            if (result.error) throw result.error;
          }}
        />
      )}
      {signup === '1' && (
        <Dialog open onOpenChange={() => setSignup(null)}>
          <DialogContent>
            <MyRegistrationsDialog eventId={event.id} initialPage="candidates" />
          </DialogContent>
        </Dialog>
      )}

      <div className="col-feature">
        <PageHeader
          imageUrl={event.coverImage?.url}
          onEditImage={
            auth.canManageEvent(event) || event.coverImage?.url || event.type === 'CAMP'
              ? () => setEditingImage(true)
              : undefined
          }
          title={formatEventName(event) || ''}
          breadcrumbLabel="Termín"
          breadcrumbs={[
            { label: 'Akce', href: '/akce' },
            ...(event.seriesId
              ? [{ label: 'Termíny', href: `/terminy/${event.seriesId}` }]
              : []),
          ]}
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
          <BasicEventInfo event={event} />
        </Tab>
        {(auth.isTrainer || (auth.isLoggedIn && numRegistrations > 0)) && (
          <Tab id="registrations" title={`Přihlášky (${numRegistrations})`}>
            <div className="col-popout">
              <EventRegistrations event={event} />
            </div>
          </Tab>
        )}
        {event.type === 'CAMP' && event.childEventsList.length > 0 && (
          <Tab id="lessons" title="Lekce" requireTrainer>
            <div className="col-full-width relative">
              <CampLessonsTable id={event.id} />
            </div>
          </Tab>
        )}
        {event.type === 'CAMP' && event.childEventsList.length > 0 && (
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
