import {
  EventFragment,
  EventInstanceApproxPriceDocument,
  EventInstanceWithTrainerFragment,
} from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { cn } from '@/lib/cn';
import {
  formatDefaultEventName,
  formatEventType,
  formatRegistrant,
  moneyFormatter,
  shortTimeFormatter,
} from '@/ui/format';
import { useActions } from '@/lib/actions';
import { eventInstanceActions } from '@/lib/actions/eventInstance';
import { ActionGroup } from '@/ui/ActionGroup';
import { Clock, Coins, MapPin, User, Users } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { isTruthy } from '@/lib/truthyFilter';

export function EventSummary({
  event,
  instance,
  offsetButtons,
}: {
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
  offsetButtons?: boolean;
}) {
  const actions = useActions(eventInstanceActions, instance);
  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const locationLabel = instance.location?.name ?? instance.locationText;

  const instancesBefore = event.eventInstancesList.filter(
    (e) => e.since < instance.since,
  ).length;

  return (
    <div className="flex flex-col gap-2 text-sm">
      {offsetButtons && (
        <div className="mt-2 flex flex-col">
          <Link
            href={{ pathname: '/akce/[id]', query: { id: event.id } }}
            className={cn('text-xl', instance.isCancelled ? 'line-through' : 'underline')}
          >
            {formatDefaultEventName(event)}
          </Link>

          <div>
            {formatEventType(instance.type)}
            {event.eventInstancesList.length > 1
              ? ` (${instancesBefore + 1}. z ${event.eventInstancesList.length} opakování)`
              : ' (jednorázová)'}
          </div>
        </div>
      )}

      <div className="flex items-center gap-2">
        <Clock className="size-5 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {locationLabel && (
        <div className="flex items-center gap-2">
          <MapPin className="size-5 text-accent-11" />
          {locationLabel}
        </div>
      )}

      {instance.trainersList?.length ? (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-5 text-accent-11 shrink-0" />
          {instance.trainersList
            .map((x) => x.person?.name)
            .filter(Boolean)
            .join(', ')}
        </div>
      ) : null}

      {instance.type === 'LESSON' && <EventInstancePriceView id={instance.id} />}

      <div className="flex items-center gap-2">
        <Users className="size-5 text-accent-11" />
        <div>
          {event.eventTargetCohortsList.length > 0 ? (
            event.eventTargetCohortsList.map((x) => x.cohort?.name).join(', ')
          ) : registrationCount === 0 ? (
            <div>VOLNÁ</div>
          ) : myRegistrations.length > 0 ? (
            [
              ...myRegistrations.map((reg) => (
                <div key={reg.id}>{formatRegistrant(reg)}</div>
              )),
              ...(registrationCount > myRegistrations.length
                ? [
                    <div key="more">
                      a dalších {registrationCount - myRegistrations.length} účastníků
                    </div>,
                  ]
                : []),
            ]
          ) : registrationCount < 6 ? (
            event.eventRegistrations.nodes.map((x) => (
              <div key={x.id}>{formatRegistrant(x)}</div>
            ))
          ) : (
            `${registrationCount} účastníků`
          )}
        </div>
      </div>

      <MyRegistrationsDialog event={event} />

      <div className={cn('absolute top-4 z-[100]', offsetButtons ? 'right-9' : 'right-2')}>
        <ActionGroup variant="row" align="end" actions={actions} />
      </div>
    </div>
  );
}

function EventInstancePriceView({ id }: { id: string }) {
  const [response] = useQuery({
    query: EventInstanceApproxPriceDocument,
    variables: { id },
  });

  const priceString = (response.data?.eventInstance?.approxPriceList ?? [])
    .filter(isTruthy)
    .filter((x) => !Number.isNaN(x.amount) && x.amount !== 'NaN')
    .map((price) => moneyFormatter.format(price))
    .join(', ');

  if (!priceString) return null;
  return (
    <div className="flex items-center gap-2" key="money">
      <Coins className="size-5 text-accent-11 shrink-0" />
      {priceString + ' / osobu'}
    </div>
  );
}
