import { formatWeekDay } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { Card } from './Card';
import { WeekPicker } from './WeekPicker';
import { EventInstanceWithEventFragment, MyEventInstanceRangeDocument } from '@/graphql/Event';
import { EventButton } from './EventButton';

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
    const eventsPerDay: { [day: string]: EventInstanceWithEventFragment[] } = {};
    data?.list?.forEach((instance) => {
      const date = startOf(new Date(instance.since), 'day');
      const location = instance.event?.location?.name || instance.event?.locationText;
      const key = date ? `${location} ${formatWeekDay(date)}` : location ?? '';
      eventsPerDay[key] = eventsPerDay[key] || [];
      eventsPerDay[key]!.push(instance);
    });
    return eventsPerDay;
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      {!fetching && !data?.list?.length && (
        <div className="text-neutral-11">Žádné akce tento týden</div>
      )}

      <div className="flex flex-wrap flex-col gap-x-2">
        {Object.entries(eventsPerDay).map(([key, eventInstances]) => (
          <Card key={key} className="grid w-72 rounded-lg border-neutral-6 border px-1 py-3">
            <h6 className="ml-3">
              <div className="font-bold mb-1">{key.split(' ')[1]!}</div>
              <div className="text-sm text-neutral-11">{key.split(' ')[0]!}</div>
            </h6>
            {eventInstances.map((instance) => (
              <EventButton key={instance.id} instance={instance} viewer='auto' />
            ))}
          </Card>
        ))}
      </div>
    </div>
  );
};
