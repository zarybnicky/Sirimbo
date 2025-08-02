import {
  EventFragment,
  EventInstanceFragment,
  MyEventInstanceRangeDocument,
} from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { formatWeekDay } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { cardCls } from '../style';

type EventPair = { event: EventFragment; instance: EventInstanceFragment; };

export function MyEventsList() {
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data, fetching }] = useQuery({
    query: MyEventInstanceRangeDocument,
    variables: {
      start: startDate.toISOString(),
      end: add(startDate, 1, 'week').toISOString(),
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
        const minTime = Math.min(...items.map(x => new Date(x.instance.since).getTime()));
        const sortKey = `${date}T${minTime}`;
        return [date, sortKey, location, items] as const;
      }),
    );
    return list.sort((x, y) => x[1].localeCompare(y[1]));
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      {fetching ? (
        <div className="text-neutral-11">Načítám...</div>
      ) : (data?.list?.length ? null : (
        <div className="text-neutral-11">Žádné nadcházející akce</div>
      ))}

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsPerDay.map(([date, _, location, eventInstances]) => (
          <div
            key={`${date}_${location}`}
            className={cardCls({ className: 'rounded-lg border-neutral-6 border px-1 py-3' })}
          >
            <h6 className="ml-3">
              <div className="font-bold mb-1">{formatWeekDay(new Date(date))}</div>
              <div className="text-sm text-neutral-11">{location}</div>
            </h6>
            {eventInstances.map(({ event, instance }) => (
              <EventButton key={instance.id} event={event} instance={instance} viewer="auto" />
            ))}
          </div>
        ))}
      </div>
    </div>
  );
}
