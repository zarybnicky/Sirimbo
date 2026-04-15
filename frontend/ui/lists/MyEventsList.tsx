import {
  EventFragment,
  EventInstanceRangeDocument,
  EventInstanceWithTrainerFragment,
} from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { formatWeekDay } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { buttonCls, cardCls } from '../style';
import { useAuth } from '../use-auth';
import Link from 'next/link';
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, LucideIcon, OctagonMinus, X } from 'lucide-react';

type EventPair = { event: EventFragment; instance: EventInstanceWithTrainerFragment };

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus,
};

export function MyEventsList() {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data, fetching }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      start: startDate.toISOString(),
      end: add(startDate, 1, 'week').toISOString(),
      onlyMine: true,
    },
  });

  const eventsPerDay = React.useMemo(() => {
    const map = new Map<string, Map<string, EventPair[]>>();
    for (const instance of data?.list || []) {
      const { event } = instance;
      if (!event) continue;

      const date = startOf(new Date(instance.since), 'day').toISOString();
      const location = event.location?.name || event.locationText || '';

      const locations = map.get(date) ?? new Map<string, EventPair[]>();
      locations.set(location, [...(locations.get(location) ?? []), { event, instance }]);
      map.set(date, locations);
    }
    const list = [...map.entries()].flatMap(([date, itemMap]) =>
      [...itemMap.entries()].map(([location, items]) => {
        items.sort((x, y) => x.instance.since.localeCompare(y.instance.since));
        const minTime = Math.min(
          ...items.map((x) => new Date(x.instance.since).getTime()),
        );
        const sortKey = `${date}T${minTime}`;
        return [date, sortKey, location, items] as const;
      }),
    );
    return list.toSorted((x, y) => x[1].localeCompare(y[1]));
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      {fetching ? (
        <div className="text-neutral-11">Načítám...</div>
      ) : data?.list?.length ? null : (
        <div className="text-neutral-11">Žádné nadcházející akce</div>
      )}

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsPerDay.map(([date, _, location, eventInstances]) => (
          <div
            key={`${date}_${location}`}
            className={cardCls({
              className: 'rounded-lg border-neutral-6 border px-1 py-3',
            })}
          >
            <h6 className="ml-3">
              <div className="font-bold mb-1">{formatWeekDay(new Date(date))}</div>
              <div className="text-sm text-neutral-11">{location}</div>
            </h6>
            {eventInstances.map(({ event, instance }) => (
              <React.Fragment key={instance.id}>
                <EventButton event={event} instance={instance} viewer="auto" />
                {event.type === 'GROUP' &&
                  (auth.isAdmin ||
                    auth.personIds.find((id) =>
                      instance.managerPersonIds.includes(id),
                    )) && (
                    <Link
                      className={buttonCls({
                        size: 'sm',
                        variant: 'outline',
                        className: 'ml-6 mb-2 inline-flex items-center',
                      })}
                      href={{
                        pathname: '/akce/[id]/termin/[instance]',
                        query: {
                          id: event.id,
                          instance: instance.id,
                        },
                      }}
                    >
                      <span>Docházka </span>
                      <span className="inline-flex items-center tabular-nums">
                        <span className="bg-green-3 text-sm px-1 rounded-l-xl text-green-11">
                          <labels.ATTENDED className="inline" />{' '}
                          {JSON.parse(instance.stats)['ATTENDED']}
                        </span>
                        <span className=" bg-[#fbe4e8] text-sm px-1 rounded-r-xl  text-[#b42346] dark:bg-[#471823] dark:text-[#ffb4c2]">
                          {JSON.parse(instance.stats)['NOT_EXCUSED']}{' '}
                          <labels.NOT_EXCUSED className="inline" />
                        </span>
                      </span>
                      <span className="lowercase">
                        z {JSON.parse(instance.stats)['TOTAL']}
                      </span>
                    </Link>
                  )}
              </React.Fragment>
            ))}
          </div>
        ))}
      </div>
    </div>
  );
}
