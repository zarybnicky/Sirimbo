import { formatRegistrant } from '@/ui/format';
import { shortTimeFormatter } from '@/ui/format';
import { EventInstanceWithEventFragment } from '@/graphql/Event';
import { Clock, MapPin, User, Users } from 'lucide-react';
import Link from 'next/link';
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { buttonCls } from './style';
import React from 'react';

export function EventSummary({ instance }: {
  instance: EventInstanceWithEventFragment;
}) {
  const event = instance.event;

  if (!event) return null;

  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  return (
    <div className="flex flex-col gap-2 text-sm">
      <div className="flex items-center gap-2">
        <Clock className="w-6 h-6 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {event.location && (
        <div className="flex items-center gap-2">
          <MapPin className="w-6 h-6 text-accent-11" />
          {event.location.name}
        </div>
      )}
      {event.locationText && (
        <div className="flex items-center gap-2">
          <MapPin className="w-6 h-6 text-accent-11" />
          {event.locationText}
        </div>
      )}

      {event.eventTrainersList.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="w-6 h-6 text-accent-11 shrink-0" />
          {event.eventTrainersList.map((x) => x.person?.name).join(', ')}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="w-6 h-6 text-accent-11" />
        <span>
          {event.eventTargetCohortsList.length > 0 ? (
            event.eventTargetCohortsList.map(x => (
              <div key={x.id}>{x.cohort?.sName}</div>
            ))
          ) : registrationCount  === 0 ? (
            <div>VOLNÁ</div>
          ) : myRegistrations.length > 0 ? (
            myRegistrations.map((reg) => (
              <div key={reg.id}>{formatRegistrant(reg)}</div>
            )).concat(
              registrationCount > myRegistrations.length ? [(
                <div key="more">a dalších {registrationCount - myRegistrations.length} účastníků</div>
              )] : []
            )
          ) : registrationCount < 6 ? (
            event.eventRegistrations.nodes.map(x => (
              <div key={x.id}>{formatRegistrant(x)}</div>
            ))
          ) : (
            `${registrationCount} účastníků`
          )}
        </span>
      </div>

      <div className="flex flex-wrap gap-3">
        <MyRegistrationsDialog event={event} />
        <Link href={`/akce/${event.id}`} className={buttonCls({ variant: 'outline' })}>
          Více info...
        </Link>
      </div>
    </div>
  );
}
