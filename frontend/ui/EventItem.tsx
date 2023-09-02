import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument, EventWithRegistrationsFragment, EventWithAttendanceFragment, EventWithRegistrantsFragment } from '@app/graphql/Event';
import { formatOpenDateRange, shortDateFormatter } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { formatDefaultEventName, formatEventType, formatRegistrant } from '@app/ui/format';
import { TitleBar } from './TitleBar';
import { EditEventDialog } from './EditEventDialog';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { AttendanceType } from '@/graphql';
import { PersonFragment } from '@/graphql/Person';

export const EventItem = ({ id }: { id: string }) => {
  const { user, perms } = useAuth();
  const [variant, setVariant] = useQueryParam('tab', withDefault(StringParam, 'info'));
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
  if ((event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'registrations',
      label: `Přihlášky (${event.eventRegistrationsList.length ?? 0})`,
      contents: <Registrations key="registrations" event={event} />
    });
  }
  if ((event.eventRegistrationsList?.length ?? 0) > 0) {
    tabs.push({
      id: 'attendance',
      label: `Účast`,
      contents: <Attendance key="attendance" event={event} />
    });
  }

  return (
    <>
      <TitleBar title={event.name || formatDefaultEventName(event)}>
        {perms.isAdmin && (
          <EditEventDialog id={id} />
        )}
      </TitleBar>
      <BasicInfo event={event} />

      <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
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

function Attendance({ event }: { event: EventWithAttendanceFragment & EventWithRegistrantsFragment }) {

  //                   instances
  //                   ---------
  //               |
  // registrations |  attendance

  const data = React.useMemo(() => {
    const data: { [key: string]: {
      person: PersonFragment;
      instances: { [key: string]: { id?: string; status: AttendanceType } }
    }} = {};
    for (const instance of event.eventInstancesList) {
      for (const person of (event.registrantsList ?? [])) {
        if (!data[person.id]) {
          data[person.id] = { person, instances: {} };
        }
        data[person.id]!.instances[instance.id] = { status: 'UNKNOWN' };
      }
      for (const attendance of instance.eventAttendancesByInstanceIdList) {
        data[attendance.personId]!.instances[instance.id] = { status: attendance.status };
      }
    }
    return data;
  }, [event]);

  return (
    <div className="prose prose-accent">
      <table>
        <thead>
          <tr>
            <th></th>
            {event.eventInstancesList.map(instance => (
              <th key={instance.id}>{shortDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {Object.values(data).map(x => (
            <tr key={x.person.id}>
              <td>{x.person.name}</td>
              {Object.values(x.instances).map((y, i) => (
                <td key={i}>{y.status}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

function BasicInfo({ event }: { event: EventWithRegistrationsFragment }) {
  return (
    <dl className="gap-2 mb-6">
      <dt>{formatEventType(event)}</dt>
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

      <dt>Místo konání</dt>
      <dd>{event.locationText}</dd>

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
