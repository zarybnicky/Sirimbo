import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument, EventWithRegistrationsFragment, EventWithAttendanceFragment, EventWithRegistrantsFragment, UpdateAttendanceDocument, EventAttendanceFragment } from '@app/graphql/Event';
import { formatOpenDateRange, shortDateFormatter } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useMutation, useQuery } from 'urql';
import { formatDefaultEventName, formatEventType, formatRegistrant } from '@app/ui/format';
import { TitleBar } from './TitleBar';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { AttendanceType } from '@/graphql';
import { PersonFragment } from '@/graphql/Person';
import { DropdownMenu, DropdownMenuContent, DropdownMenuTrigger } from './dropdown';
import { buttonCls } from './style';
import { CheckIcon, ChevronDown } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { DropdownMenuItemIndicator, DropdownMenuRadioGroup, DropdownMenuRadioItem } from '@radix-ui/react-dropdown-menu';

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
      <TitleBar title={event.name || formatDefaultEventName(event)} />
      <BasicInfo event={event} />

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
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
  const { perms } = useAuth();

  const data = React.useMemo(() => {
    const data: { [key: string]: {
      person: PersonFragment;
      instances: { [key: string]: Omit<EventAttendanceFragment, 'id' | 'registrationId'> }
    }} = {};
    for (const instance of event.eventInstancesList) {
      for (const person of (event.registrantsList ?? [])) {
        if (!data[person.id]) {
          data[person.id] = { person, instances: {} };
        }
        data[person.id]!.instances[instance.id] = {
          status: 'UNKNOWN',
          instanceId: instance.id,
          personId: person.id,
          note: null,
        };
      }
      for (const attendance of instance.eventAttendancesByInstanceIdList) {
        data[attendance.personId]!.instances[instance.id] = attendance;
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
            {event.eventInstancesList.map((instance) => (
              <th className="text-center" key={instance.id}>
                {shortDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {Object.values(data).map(reg => (
            <tr key={reg.person.id}>
              <td>{reg.person.name}</td>
              {Object.entries(reg.instances).map(([instanceId, attendance]) => (
                perms.isTrainerOrAdmin ? (
                  <AttendanceItem key={instanceId} attendance={attendance} />
                ) : (
                  <div className="text-center" key={instanceId}>
                    {labels[attendance.status]}
                  </div>
                )
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

const labels: { [key in AttendanceType]: string} = {
  ATTENDED: 'zúčastnil(a) se',
  EXCUSED: 'omluven(a)',
  NOT_EXCUSED: 'nedostavil(a) se',
  UNKNOWN: '?',
}
function isAttendanceType(x: string): x is AttendanceType {
  return ['ATTENDED', 'EXCUSED', 'NOT_EXCUSED', 'UNKNOWN'].includes(x);
}

function AttendanceItem({ attendance }: { attendance: Partial<EventAttendanceFragment> }) {
  const status = attendance.status || 'UNKNOWN';
  const label = labels[status];
  const update = useMutation(UpdateAttendanceDocument)[1];
  const setStatus = useAsyncCallback(async (status: string) => {
    if (isAttendanceType(status)) {
      await update({
        input: {
          status,
          instanceId: attendance.instanceId,
          note: attendance.note,
          personId: attendance.personId,
        },
      })
    }
  });

  return (
    <DropdownMenu>
      <td className="text-center">
        <DropdownMenuTrigger asChild>
          <button type="button" className={buttonCls({ className: 'w-full justify-between max-w-[10rem]', size: 'sm', variant: 'outline' })}>
            {label}
            <ChevronDown />
          </button>
        </DropdownMenuTrigger>
      </td>
      <DropdownMenuContent align="end">
        <DropdownMenuRadioGroup value={attendance.status} onValueChange={setStatus.execute}>
          {Object.entries(labels).map(([key, label]) => (
            <DropdownMenuRadioItem key={key} value={key} className="flex justify-between p-1">
              {label}
              <DropdownMenuItemIndicator>
                <CheckIcon />
              </DropdownMenuItemIndicator>
            </DropdownMenuRadioItem>
          ))}
        </DropdownMenuRadioGroup>
      </DropdownMenuContent>
    </DropdownMenu>
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
