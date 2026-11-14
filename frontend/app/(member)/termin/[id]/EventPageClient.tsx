'use client';

import { CampSchedule } from '@/calendar/CampSchedule';
import { CampLessonsTable } from '@/calendar/CampLessonsTable';
import { CampTrainersTable } from '@/calendar/CampTrainersTable';
import {
  EventWithAttendanceDocument,
  EventPaymentsDocument,
  UpdateEventDocument,
  type EventWithAttendanceQuery,
} from '@/graphql/Event';
import { eventActions } from '@/lib/actions/event';
import { useActions } from '@/lib/actions';
import { EventRegistrations } from '@/ui/EventRegistrations';
import { Layout } from '@/ui/Layout';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/PageHeader';
import { Dialog, DialogContent } from '@/ui/dialog';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { dateTimeFormatter, formatEventName, formatEventType } from '@/ui/format';
import { useAuth } from '@/lib/auth';
import { parseAsString, useQueryState } from 'nuqs';
import { useMutation, useQuery } from 'urql';
import React from 'react';
import { ImageEditorDialog } from '@/ui/ImageEditorDialog';
import { PaymentTable } from '@/app/(member)/platby/PaymentTable';
import { RichTextView } from '@/ui/RichTextView';
import { ListFormat } from '@/ui/ListFormat';
import { EntityLink, getEntityLink } from '@/ui/EntityLink';
import { DotSeparated } from '@/ui/DotSeparated';

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

  const trainers = event.eventTrainersList.flatMap((t) => t.person ?? []);
  const { seriesInfo, registrationInfo } = event;
  const numRegistrations = registrationInfo?.registrations ?? 0;
  const hasPayments = [event, ...event.childEventsList].some(
    (e) => e.paymentsList.length > 0,
  );

  const subtitleParts = [
    event.isCancelled && 'Zrušeno',
    dateTimeFormatter.formatRange(new Date(event.since), new Date(event.until)),
    (event.location?.name || event.locationText) && (
      <EntityLink type="event-location" entity={event} />
    ),
  ].filter(Boolean);

  const subtitlePartsSecondary = [
    <div className="" key="trainers">
      <ListFormat>
        {trainers.map((x) => (
          <EntityLink key={x.id} type="person" entity={x} />
        ))}
      </ListFormat>
    </div>,
    !!event.capacity &&
      `Kapacita: zbývá ${registrationInfo?.remainingCapacity} míst z ${event.capacity}`,
  ].filter(Boolean);

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
          title={formatEventName(event)}
          breadcrumbLabel={
            seriesInfo && (seriesInfo.length ?? 0) > 1
              ? `${seriesInfo.position}. ${formatEventType(event.type).toLowerCase()} z ${seriesInfo.length}`
              : formatEventType(event.type)
         }
          breadcrumbs={[
            { title: 'Události', href: '/akce' },
            ...(event.parentId
              ? [
                  getEntityLink('event', {
                    id: event.parentId,
                    name: event.parent?.name ?? 'Soustředění',
                  }),
                ]
              : []),
            ...(event.seriesId
              ? [
                  {
                    title: `Série ${event.seriesInfo?.name}`,
                    href: `/terminy/${event.seriesId}`,
                  },
                ]
              : []),
          ]}
          subtitle={
            <div className="flex flex-col gap-2">
              <DotSeparated>{subtitleParts}</DotSeparated>
              <DotSeparated>{subtitlePartsSecondary}</DotSeparated>
            </div>
          }
          actions={actions}
          primary={primaryAction}
        />
      </div>

      <TabMenu
        className="col-feature"
        selected={tab === 'attendance' ? 'registrations' : tab}
        onSelect={setTab}
      >
        {(event.summary || event.description) && (
          <Tab id="info" title="Info">
            <div className="flex flex-col gap-3">
              <RichTextView value={event.summary} />
              <RichTextView value={event.description} />
            </div>
          </Tab>
        )}

        {event.type === 'CAMP' &&
          (auth.isLoggedIn || event.hasPublicDetails || hasShareToken) && (
            <Tab id="schedule" title="Rozpis">
              <CampSchedule event={event} />
            </Tab>
          )}

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

const EventPayments = React.memo(function EventPayments({ id }: { id: string }) {
  const [{ data, fetching, error }] = useQuery({
    query: EventPaymentsDocument,
    variables: { id },
  });

  const event = data?.event;
  const payments = event
    ? [event, ...event.childEventsList].flatMap((e) => e.paymentsList)
    : [];

  return <PaymentTable payments={payments} busy={fetching} error={error} />;
});
