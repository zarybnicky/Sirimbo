import { DotSeparated } from '@/ui/DotSeparated';
import { EventWithTrainerFragment } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import {
  formatEventName,
  formatEventType,
  formatParticipant,
  listFormatter,
  moneyFormatter,
  shortTimeFormatter,
} from '@/ui/format';
import { useActions } from '@/lib/actions';
import { eventActions } from '@/lib/actions/event';
import { ActionGroup } from '@/ui/ActionGroup';
import { Clock, Coins, MapPin, User, Users } from 'lucide-react';
import Link from 'next/link';
import { isTruthy } from '@/lib/truthyFilter';
import React from 'react';
import { startOf } from 'date-arithmetic';

export function EventSummary({ event }: { event: EventWithTrainerFragment }) {
  const actions = useActions(eventActions, event);
  const { seriesInfo, registrations } = event;
  const registrationCount = event.registrationInfo?.registrations ?? 0;
  const locationLabel = event.location?.name || event.locationText;
  const cohorts = event.targetCohortsList.flatMap((x) => (x.cohort ? [x.cohort] : []));
  const priceString = (event.approxPriceList ?? [])
    .filter(isTruthy)
    .filter((x) => !Number.isNaN(x.amount) && x.amount !== 'NaN')
    .map((price) => moneyFormatter.format(price))
    .join(', ');

  const primaryActions = React.useMemo(() => {
    return [
      startOf(new Date(event.since), 'day') > startOf(new Date(), 'day')
        ? 'event.registrations'
        : 'event.attendance',
      'event.edit',
    ];
  }, [event.since]);

  return (
    <div className="flex flex-col gap-2 text-sm">
      <DotSeparated className="-mb-2 text-sm text-accent-11">
        <span>{formatEventType(event.type)}</span>
        {event.parent && (
          <Link
            href={`/termin/${event.parent.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {event.parent.name}
          </Link>
        )}
        {seriesInfo?.id && seriesInfo.length !== null && seriesInfo.length > 1 && (
          <Link
            href={`/terminy/${seriesInfo.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {seriesInfo.position}. z {seriesInfo.length} v sérii
          </Link>
        )}
      </DotSeparated>

      <Link
        href={`/termin/${event.id}`}
        className={cn(
          'block text-xl',
          event.isCancelled ? 'line-through' : 'underline decoration-accent-12/50',
        )}
      >
        {formatEventName(event)}
      </Link>

      <div className="flex items-center gap-2">
        <Clock className="size-4 text-accent-11 shrink-0" />
        {shortTimeFormatter.formatRange(
          new Date(event.since),
          new Date(event.until),
        )}
      </div>

      {locationLabel && (
        <div className="flex items-center gap-2">
          <MapPin className="size-4 text-accent-11 shrink-0" />
          {event.location ? (
            <Link className="underline" href={`/lokality/${event.location.id}`}>
              {event.location.name}
            </Link>
          ) : (
            locationLabel
          )}
        </div>
      )}

      {!!event.eventTrainersList?.length && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-4 text-accent-11 shrink-0" />
          {listFormatter.format(
            event.eventTrainersList.map((x) => x.person?.name).filter(isTruthy),
          )}
        </div>
      )}

      {priceString && (
        <div className="flex items-center gap-2">
          <Coins className="size-4 text-accent-11 shrink-0" />
          {priceString + ' / osobu'}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="size-4 text-accent-11 shrink-0" />
        <div>
          {cohorts.length > 0 ? (
            listFormatter.format(cohorts.map((x) => x.name))
          ) : registrationCount === 0 ? (
            <div>VOLNÁ</div>
          ) : registrationCount < 6 ? (
            registrations.nodes.map((x) => <div key={x.id}>{formatParticipant(x)}</div>)
          ) : (
            `${registrationCount} účastníků`
          )}
        </div>
      </div>

      <ActionGroup
        className="max-w-full flex-wrap"
        primary={primaryActions}
        actions={actions}
      />
    </div>
  );
}
