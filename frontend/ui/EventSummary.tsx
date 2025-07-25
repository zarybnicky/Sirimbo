import { EventFragment, EventInstanceApproxPriceDocument, EventInstanceFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { cn } from "@/ui/cn";
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { formatDefaultEventName, formatEventType, formatRegistrant, moneyFormatter, shortTimeFormatter } from '@/ui/format';
import { EventInstanceMenu } from '@/ui/menus/EventInstanceMenu';
import { Clock, MapPin, User, Users, Coins } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { truthyFilter } from './truthyFilter';

export function EventSummary({ event, instance, offsetButtons }: {
  event: EventFragment;
  instance: EventInstanceFragment;
  offsetButtons?: boolean;
}) {
  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  const instancesBefore = event.eventInstancesList.filter(e => e.since < instance.since).length;

  return (
    <div className="flex flex-col gap-2 text-sm">
      {offsetButtons && (<div className="mt-2 flex flex-col">
        <Link
        href={{ pathname: '/akce/[id]', query: { id: event.id } }}
          className={cn("text-xl", (instance.isCancelled ? "line-through" : "underline"))}
        >
          {formatDefaultEventName(event)}
        </Link>

        <div>
          {formatEventType(event)}
          {event.eventInstancesList.length > 1
            ? ` (${instancesBefore + 1}. z ${event.eventInstancesList.length} opakování)`
            : ' (jednorázová)'}
        </div>
      </div>)}


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

      {event.eventTrainersList.length > 0 && event.eventRegistrations.totalCount > 0 && event.type === 'LESSON' && (
        <div className="flex items-center gap-2" key="money">
          <Coins className="size-5 text-accent-11 shrink-0" />
          <EventInstancePriceView id={instance.id} />
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
            [
              ...myRegistrations.map((reg) => (
                <div key={reg.id}>{formatRegistrant(reg)}</div>
              )),
              ...(registrationCount > myRegistrations.length ? [(
                <div key="more">a dalších {registrationCount - myRegistrations.length} účastníků</div>
              )] : [])]
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

      <EventInstanceMenu className="z-[100]" align="end" event={event} instance={instance}>
        <DropdownMenuTrigger.RowDots className={cn("size-5 absolute top-4", offsetButtons ? "right-9" : "right-2")} />
      </EventInstanceMenu>
    </div>
  );
}

function EventInstancePriceView({ id }: { id: string }) {
  const [response] = useQuery({ query: EventInstanceApproxPriceDocument, variables: { id } });
  return (
    (response.data?.eventInstance?.approxPriceList ?? [])
      .filter(truthyFilter)
      .map(price => (
        price.currency === 'CZK' ? moneyFormatter.format(price.amount) : `${price.amount} ${price.currency}`
      ))
      .join(', ')
    + ' / osobu'
  );
}
