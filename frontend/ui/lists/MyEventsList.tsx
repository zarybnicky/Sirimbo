import { EventInstanceRangeDocument, EventWithTrainerFragment } from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { weekDayFormatter } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { cardCls } from '../style';

export function MyEventsList() {
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
    const map = new Map<string, Map<string, EventWithTrainerFragment []>>();
    for (const instance of data?.list || []) {
      const date = startOf(new Date(instance.since), 'day').toISOString();
      const location = instance.location?.name || instance.locationText || '';

      const locations = map.get(date) ?? new Map<string, EventWithTrainerFragment[]>();
      locations.set(location, [...(locations.get(location) ?? []), instance]);
      map.set(date, locations);
    }
    const list = [...map.entries()].flatMap(([date, itemMap]) =>
      [...itemMap.entries()].map(([location, items]) => {
        items.sort((x, y) => x.since.localeCompare(y.since));
        const minTime = Math.min(
          ...items.map((x) => new Date(x.since).getTime()),
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

      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!fetching && !data?.list?.length ? 'Žádné nadcházející akce' : ''}
      </div>

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsPerDay.map(([date, _, location, eventInstances]) => (
          <div
            key={`${date}_${location}`}
            className={cardCls({
              className: 'rounded-lg border-neutral-6 border px-1 py-3',
            })}
          >
            <h6 className="ml-3">
              <div className="font-bold mb-1 capitalize">
                {weekDayFormatter.format(new Date(date))}
              </div>
              <div className="text-sm text-neutral-11">{location}</div>
            </h6>
            {eventInstances.map((instance) => (
              <EventButton
                key={instance.id}
                instance={instance}
                viewer="auto"
                attendance="inline"
              />
            ))}
          </div>
        ))}
      </div>
    </div>
  );
}
