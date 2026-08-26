import { EventApproxPriceDocument, EventWithTrainerFragment } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import {
  formatEventName,
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
import { useQuery } from 'urql';
import { isTruthy } from '@/lib/truthyFilter';
import React from 'react';

export function EventSummary({ instance }: { instance: EventWithTrainerFragment }) {
  const actions = useActions(eventInstanceActions, instance);
  const { seriesInfo, registrations } = instance;
  const registrationCount = instance.registrationInfo?.registrations ?? 0;
  const locationLabel = instance.location?.name || instance.locationText;
  const cohorts = instance.targetCohortsList.flatMap((x) => (x.cohort ? [x.cohort] : []));

  const primaryActions = React.useMemo(() => {
    return [
      new Date(instance.since) > new Date()
        ? 'eventInstance.registrations'
        : 'eventInstance.attendance',
      'eventInstance.edit',
    ];
  }, [instance.since]);

  return (
    <div className="flex flex-col gap-2 text-sm">
      <div className="-mb-2 text-sm text-accent-11 flex flex-wrap items-center gap-y-1 [&>*:not(:last-child)]:after:content-['•'] *:after:mx-1.5 *:after:text-accent-9">
        <span>{formatEventType(instance.type)}</span>
        {instance.parent && (
          <Link
            href={`/termin/${instance.parent.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {instance.parent.name}
          </Link>
        )}
        {seriesInfo?.id && seriesInfo.length !== null && seriesInfo.length > 1 && (
          <Link
            href={`/terminy/${seriesInfo.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {seriesInfo.position}. z {seriesInfo.length} v sérii {seriesInfo.name?.trim()}
          </Link>
        )}
      </div>

      <Link
        href={`/termin/${instance.id}`}
        className={cn(
          'block text-xl',
          instance.isCancelled ? 'line-through' : 'underline decoration-accent-12/50',
        )}
      >
        {formatEventName(instance)}
      </Link>

      <div className="flex items-center gap-2">
        <Clock className="size-4 text-accent-11 shrink-0" />
        {shortTimeFormatter.formatRange(
          new Date(instance.since),
          new Date(instance.until),
        )}
      </div>

      {locationLabel && (
        <div className="flex items-center gap-2">
          <MapPin className="size-4 text-accent-11 shrink-0" />
          {locationLabel}
        </div>
      )}

      {!!instance.trainersList?.length && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-4 text-accent-11 shrink-0" />
          {instance.trainersList
            .map((x) => x.person?.name)
            .filter(Boolean)
            .join(', ')}
        </div>
      )}

      {instance.type === 'LESSON' && <EventPriceView id={instance.id} />}

      <div className="flex items-center gap-2">
        <Users className="size-4 text-accent-11 shrink-0" />
        <div>
          {cohorts.length > 0 ? (
            cohorts.map((x) => x.name).join(', ')
          ) : registrationCount === 0 ? (
            <div>VOLNÁ</div>
          ) : registrationCount < 6 ? (
            registrations.nodes.map((x) => <div key={x.id}>{formatRegistrant(x)}</div>)
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

function EventPriceView({ id }: { id: string }) {
  const [response] = useQuery({
    query: EventApproxPriceDocument,
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
      <Coins className="size-4 text-accent-11 shrink-0" />
      {priceString + ' / osobu'}
    </div>
  );
}
