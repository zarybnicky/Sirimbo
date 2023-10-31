import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument, EventFragment, EventWithRegistrationsFragment } from '@app/graphql/Event';
import { formatLongCoupleName, formatOpenDateRange, fullDateFormatter } from '@app/ui/format';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { formatDefaultEventName, formatEventType } from '@app/ui/format';
import { TitleBar } from './TitleBar';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from './TabMenu';
import { UpsertEventForm } from './event-form/UpsertEventForm';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { EditEventDescriptionForm } from './EditEventDescriptionForm';
import Link from 'next/link';
import { AttendanceType } from '@/graphql';
import { Annoyed, Check, HelpCircle, LucideIcon, X } from 'lucide-react';

const labels: { [key in AttendanceType]: LucideIcon} = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  EXCUSED: Annoyed,
  NOT_EXCUSED: X,
}

export function EventView({ id }: { id: string }) {
  const { user, perms } = useAuth();
  const [variant, setVariant] = useQueryParam('tab', StringParam);
  const [editOpen, setEditOpen] = React.useState(false);
  const [descOpen, setDescOpen] = React.useState(false);

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
      contents: <div className="prose prose-accent">
        <table>
          <thead>
         <tr>
           <th></th>
      {Object.values(labels).map(x => <th className="text-center">{React.createElement(x, {className: 'inline-block'})}</th>)}
         </tr>
         </thead>
          <tbody>
        {event.eventInstancesList.map(instance => (
          <tr>
            <td>
              <Link href={`/akce/${event.id}/termin/${instance.id}`}>
                {fullDateFormatter.formatRange(new Date(instance.since), new Date(instance.until))}
              </Link>
            </td>
          {Object.keys(labels).map((status) => (
                <td className="text-center">{instance.attendanceSummaryList?.find(x => x?.status === status)?.count ?? 0}</td>
              ))}
          </tr>
        ))}
          </tbody>
        </table>
      </div>
    });
  }

  return (
    <>
      <TitleBar title={event.name || formatDefaultEventName(event)}>
        {perms.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTriggerDots />
            <DropdownMenuContent align="end">
              <Dialog open={editOpen} onOpenChange={setEditOpen}>
                <DialogTrigger asChild>
                  <DropdownMenuButton onSelect={(e) => e.preventDefault()}>
                    Upravit
                  </DropdownMenuButton>
                </DialogTrigger>
                <DialogContent>
                  <UpsertEventForm event={event} onSuccess={() => setEditOpen(false)} />
                </DialogContent>
              </Dialog>

              <Dialog open={descOpen} onOpenChange={setDescOpen}>
                <DialogTrigger asChild>
                  <DropdownMenuButton onSelect={(e) => e.preventDefault()}>
                    Upravit dlouhý popis
                  </DropdownMenuButton>
                </DialogTrigger>
                <DialogContent>
                  <EditEventDescriptionForm event={event} onSuccess={() => setDescOpen(false)} />
                </DialogContent>
              </Dialog>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </TitleBar>

      <BasicEventInfo event={event} />

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
        <div key={x.id} className="p-1">
          <div>{x.person ? x.person.name || '' : formatLongCoupleName(x.couple!)}</div>
          {(x.note || x.eventLessonDemandsByRegistrationIdList) && (
            <div className="ml-3">
              {x.eventLessonDemandsByRegistrationIdList.map(x => (
                <div key={x.id}>
                  {x.lessonCount}x {event.eventTrainersList.find(y => y.id === x.trainerId)?.person?.name}
                </div>
              ))}
              {x.note}
            </div>
          )}
        </div>
      ))}
    </div>
  );
}

export function BasicEventInfo({ event }: { event: EventFragment }) {
  return (
    <dl className="not-prose gap-2 mb-6">
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

      {!!event.location?.name && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.location.name}</dd>
        </>
      )}
      {!!event.locationText && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.locationText}</dd>
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
