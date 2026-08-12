import { EventRangeDocument } from '@/graphql/Event';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { capitalize, longDayFormatter } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { cardCls } from '../style';

export function MyEventsList() {
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data, fetching }] = useQuery({
    query: EventRangeDocument,
    variables: {
      start: startDate.toISOString(),
      end: add(startDate, 1, 'week').toISOString(),
      scope: 'MINE',
    },
  });

  const eventsByDay = React.useMemo(() => {
    const map = new Map<string, EventWithTrainerFragment[]>();
    for (const instance of data?.list ?? []) {
      const date = startOf(new Date(instance.since), 'day').toISOString();
      const items = map.get(date);
      if (items) items.push(instance);
      else map.set(date, [instance]);
    }

    return [...map.entries()]
      .map(
        ([date, items]) =>
          [date, items.toSorted((x, y) => x.since.localeCompare(y.since))] as const,
      )
      .toSorted((x, y) => x[0].localeCompare(y[0]));
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!fetching && !data?.list?.length ? 'Žádné nadcházející akce' : ''}
      </div>

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsByDay.map(([date, eventInstances]) => (
          <div
            key={date}
            className={cardCls({
              className: 'rounded-lg border-neutral-6 border px-1 py-3',
            })}
          >
            <div className="ml-3 mb-1 font-bold">
              {capitalize(longDayFormatter.format(new Date(date)))}
            </div>
            {eventInstances.map((instance, i) => {
              const location = instance.location?.name || instance.locationText || '';
              const previous = i === 0 ? null : eventInstances.at(i - 1);
              const previousLocation = previous
                ? previous.location?.name || previous.locationText || ''
                : null;

              return (
                <React.Fragment key={instance.id}>
                  {!!location && location !== previousLocation && (
                    <div className="ml-2.5 mt-1 text-sm text-accent-11">
                      {location}
                    </div>
                  )}
                  <EventButton
                    instance={instance}
                    viewer="auto"
                    attendance="inline"
                  />
                </React.Fragment>
              );
            })}
          </div>
        ))}
      </div>
    </div>
  );
}
