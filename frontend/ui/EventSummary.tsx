import { DotSeparated } from '@/ui/DotSeparated';
import { EventWithTrainerFragment } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import {
  formatEventName,
  formatEventType,
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
import { useAuth } from '@/lib/auth';
import { ListFormat } from '@/ui/ListFormat';
import { EntityLink } from '@/ui/EntityLink';

export function EventSummary({ event }: { event: EventWithTrainerFragment }) {
  const auth = useAuth();
  const actions = useActions(eventActions, event);
  const { seriesInfo, registrations } = event;
  const registrationCount = event.registrationInfo?.registrations ?? 0;
  const cohorts = event.targetCohortsList.flatMap((x) => x.cohort ?? []);
  const trainers = event.eventTrainersList.flatMap((x) => x.person ?? []);
  const prices = event.approxPriceList?.flatMap((x) => moneyFormatter.format(x) ?? []);

  const primaryActions = React.useMemo(() => {
    return [
      !auth.canManageEvent(event) ||
      event.type === 'LESSON' ||
      startOf(new Date(event.since), 'day') > startOf(new Date(), 'day')
        ? 'event.registrations'
        : 'event.attendance',
      'event.edit',
    ].filter(isTruthy);
  }, [auth, event]);

  return (
    <div className="flex flex-col gap-2 text-sm">
      <DotSeparated className="-mb-2 text-sm text-accent-11">
        {formatEventType(event.type)}
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
        {shortTimeFormatter.formatRange(new Date(event.since), new Date(event.until))}
      </div>

      {(event.location || event.locationText) && (
        <div className="flex items-center gap-2">
          <MapPin className="size-4 text-accent-11 shrink-0" />
          <EntityLink type="event-location" entity={event} />
        </div>
      )}

      {trainers.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-4 text-accent-11 shrink-0" />
          <span>
            <ListFormat>
              {trainers.map((x) => (
                <EntityLink key={x.id} type="person" entity={x} />
              ))}
            </ListFormat>
          </span>
        </div>
      )}

      {!!prices?.length && (
        <div className="flex items-center gap-2">
          <Coins className="size-4 text-accent-11 shrink-0" />
          {prices.join(', ') + ' / osobu'}
        </div>
      )}

      {(cohorts.length > 0 ||
        registrationCount > 0 ||
        event.registrationInfo?.canRegister) && (
        <div className="flex items-center gap-2">
          <Users className="size-4 text-accent-11 shrink-0" />
          <div>
            {cohorts.length > 0 ? (
              <ListFormat>
                {cohorts.map((x) => (
                  <EntityLink key={x.id} type="cohort" entity={x} />
                ))}
              </ListFormat>
            ) : registrationCount === 0 ? (
              event.registrationInfo?.canRegister ? (
                'VOLNÁ'
              ) : (
                '-'
              )
            ) : registrationCount < 6 ? (
              registrations.nodes.map((x) => (
                <div key={x.id}>
                  <EntityLink type="participant" entity={x} />
                </div>
              ))
            ) : (
              `${registrationCount} účastníků`
            )}
          </div>
        </div>
      )}

      <ActionGroup
        className="max-w-full flex-wrap"
        primary={primaryActions}
        actions={actions}
      />
    </div>
  );
}
