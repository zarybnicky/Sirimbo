import {
  EventInstanceApproxPriceDocument,
  EventInstanceWithTrainerFragment,
} from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { cn } from '@/lib/cn';
import {
  formatDefaultInstanceName,
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

export function EventSummary({
  instance,
  offsetButtons,
}: {
  instance: EventInstanceWithTrainerFragment;
  offsetButtons?: boolean;
}) {
  const actions = useActions(eventInstanceActions, instance);
  const registrations = instance.eventId
    ? instance.registrations
    : instance.instanceRegistrations;
  const registrationCount = registrations.totalCount;
  const myRegistrations = instance.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const locationLabel = instance.location?.name || instance.locationText;

  return (
    <div className="flex flex-col gap-2 text-sm">
      {offsetButtons && (
        <div className="mt-2 flex flex-col">
          <Link
            href={`/termin/${instance.id}`}
            className={cn('text-xl', instance.isCancelled ? 'line-through' : 'underline')}
          >
            {instance.name || formatDefaultInstanceName(instance)}
          </Link>
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
          {instance.targetCohortsList && instance.targetCohortsList.length > 0 ? (
            instance.targetCohortsList.map((x) => x.cohort?.name).join(', ')
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
            registrations.nodes.map((x) => (
              <div key={x.id}>{formatRegistrant(x)}</div>
            ))
          ) : (
            `${registrationCount} účastníků`
          )}
        </div>
      </div>

      <MyRegistrationsDialog instance={instance} />

      <div
        className={cn('absolute', offsetButtons ? 'right-9 top-3.5' : 'right-2 top-2')}
      >
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
