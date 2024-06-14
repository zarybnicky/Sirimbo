import {
  type EventInstanceWithEventFragment,
  MyEventInstanceRangeDocument,
} from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { formatWeekDay } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { cardCls } from '../style';

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
    const map = new Map<string, Map<string, EventInstanceWithEventFragment[]>>();
    for (const instance of data?.list || []) {
      const date = startOf(new Date(instance.since), 'day').toISOString();
      const location =
        instance.event?.location?.name || instance.event?.locationText || '';

      const locations =
        map.get(date) ?? new Map<string, EventInstanceWithEventFragment[]>();
      locations.set(location, (locations.get(location) ?? []).concat([instance]));
      map.set(date, locations);
    }
    const list = Array.from(map.entries()).flatMap(([date, itemMap]) =>
      Array.from(itemMap.entries()).map(([location, items]) => {
        items.sort((x, y) => x.since.localeCompare(y.since));
        return [date, location, items] as const;
      }),
    );
    return list.sort((x, y) => x[0].localeCompare(y[0]));
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      {!fetching && !data?.list?.length && (
        <div className="text-neutral-11">Žádné akce tento týden</div>
      )}

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsPerDay.map(([date, location, eventInstances]) => (
          <div
            key={`${date}_${location}`}
            className={cardCls({ className: 'grid w-72 rounded-lg border-neutral-6 border px-1 py-3' })}
          >
            <h6 className="ml-3">
              <div className="font-bold mb-1">{formatWeekDay(new Date(date))}</div>
              <div className="text-sm text-neutral-11">{location}</div>
            </h6>
            {eventInstances.map((instance) => (
              <EventButton key={instance.id} instance={instance} viewer="auto" />
            ))}
          </div>
        ))}
      </div>
    </div>
  );
}
