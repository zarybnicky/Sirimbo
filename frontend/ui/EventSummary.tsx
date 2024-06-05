import type { EventInstanceWithEventFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { cn } from "@/ui/cn";
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { formatDefaultEventName, formatRegistrant, shortTimeFormatter } from '@/ui/format';
import { EventInstanceMenu } from '@/ui/menus/EventInstanceMenu';
import { useAuth } from '@/ui/use-auth';
import { Clock, MapPin, User, Users } from 'lucide-react';
import Link from 'next/link';
import React from 'react';

export function EventSummary({ instance, offsetButtons }: {
  instance: EventInstanceWithEventFragment;
  offsetButtons?: boolean;
}) {
  const auth = useAuth();
  const event = instance.event;

  if (!event) return null;

  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  return (
    <div className="flex flex-col gap-2 text-sm">
      {offsetButtons && (
        <Link href={`/akce/${event.id}`} className={cn("text-xl mt-2", (instance.isCancelled ? "line-through" : "underline"))}>
          {formatDefaultEventName(event)}
        </Link>
      )}

      <div className="flex items-center gap-2">
        <Clock className="size-5 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {event.location && (
        <div className="flex items-center gap-2">
          <MapPin className="size-5 text-accent-11" />
          {event.location.name}
        </div>
      )}
      {event.locationText && (
        <div className="flex items-center gap-2">
          <MapPin className="size-5 text-accent-11" />
          {event.locationText}
        </div>
      )}

      {event.eventTrainersList.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-5 text-accent-11 shrink-0" />
          {event.eventTrainersList.map((x) => x.name).join(', ')}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="size-5 text-accent-11" />
        <span>
          {event.eventTargetCohortsList.length > 0 ? (
            event.eventTargetCohortsList.map(x => (
              <div key={x.id}>{x.cohort?.name}</div>
            ))
          ) : registrationCount === 0 ? (
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

      <MyRegistrationsDialog event={event} />

      {(auth.isAdmin || (auth.isTrainer && event.eventTrainersList.find(x => auth.personIds.some(id => id === x.personId)))) && (
        <EventInstanceMenu className="z-[100]" align="end" data={instance}>
          <DropdownMenuTrigger.RowDots className={cn("absolute top-4", offsetButtons ? "right-8" : "right-2")} />
        </EventInstanceMenu>
      )}
    </div>
  );
}
