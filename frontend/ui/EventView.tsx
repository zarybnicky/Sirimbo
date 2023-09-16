import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument, EventWithRegistrationsFragment } from '@app/graphql/Event';
import { formatOpenDateRange } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { formatDefaultEventName, formatEventType, formatRegistrant } from '@app/ui/format';
import { TitleBar } from './TitleBar';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { AttendanceView } from './AttendanceView';

export function EventView({ id }: { id: string }) {
  const { user } = useAuth();
  const [variant, setVariant] = useQueryParam('tab', StringParam);
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;

  if (!event) return null;

  const tabs = [];
  if (event.description || (user && event.descriptionMember)) {
    tabs.push({
      id: 'info',
      label: 'Informace',
      contents: <EventInfo key="info" event={event} />
    });
  }
  if (!!user && (event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'registrations',
      label: `Přihlášky (${event.eventRegistrationsList.length ?? 0})`,
      contents: <Registrations key="registrations" event={event} />
    });
  }
  if (!!user && (event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'attendance',
      label: `Účast`,
      contents: <AttendanceView key="attendance" event={event} />
    });
  }

  return (
    <>
      <TitleBar title={event.name || formatDefaultEventName(event)} />
      <BasicInfo event={event} />

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4 relative max-w-full">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </>
  );
};

function EventInfo({ event }: { event: EventWithRegistrationsFragment }) {
  const { user } = useAuth();
  return (
    <div>
      <RichTextView value={event.description} />
      {!!user && <RichTextView value={event.descriptionMember} />}
    </div>
  );
}

function Registrations({ event }: { event: EventWithRegistrationsFragment; }) {
  const { perms } = useAuth();
  return (
    <div>
      {perms.isTrainerOrAdmin && <EventParticipantExport id={event.id} />}
      {event.eventRegistrationsList?.map((x) => (
        <div key={x.id}>{formatRegistrant(x)}</div>
      ))}
    </div>
  );
}

function BasicInfo({ event }: { event: EventWithRegistrationsFragment }) {
  return (
    <dl className="gap-2 mb-6">
      <dd>{formatEventType(event)}</dd>
      <dt>Termíny</dt>
      <dd>
        {event.eventInstancesList.map(formatOpenDateRange).join(', ')}
      </dd>

      {event.capacity > 0 && (
        <>
          <dt>Kapacita</dt>
          <dd>Zbývá {event.remainingPersonSpots} míst z {event.capacity}</dd>
        </>
      )}

      {event.eventTrainersList.length > 0 && (
        <>
          <dt>Trenéři</dt>
          {event.eventTrainersList.map((trainer) => (
            <dd key={trainer.id}>
              {trainer.person!.firstName} {trainer.person!.lastName}{' '}
              {trainer.lessonsOffered > 0 &&
               `(zbývá ${trainer.lessonsRemaining} z ${trainer.lessonsOffered} lekcí)`}
            </dd>
          ))}
        </>
      )}

      {!!event.locationText && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.locationText}</dd>
        </>
      )}

      <dt>
        <MyRegistrationsDialog event={event} />
      </dt>

      {event.summary?.trim() && (
        <>
          <dt>Shrnutí</dt>
          <dd>
            <RichTextView value={event.summary} />
          </dd>
        </>
      )}
    </dl>
  )
}
