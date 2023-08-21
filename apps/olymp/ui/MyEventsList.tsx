import { RichTextView } from '@app/ui/RichTextView';
import { formatWeekDay } from '@app/ui/format-date';
import { useAuth } from '@app/ui/use-auth';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { Card } from './Card';
import { WeekPicker } from './WeekPicker';
import { EventInstanceExtendedFragment, EventInstanceRangeDocument } from '@app/graphql/Event';
import { EventButton } from './EventButton';

export function MyEventsList() {
  const { cohorts } = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data, fetching }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      onlyMine: true,
      start: startDate.toISOString(),
      end: add(startDate, 1, 'week').toISOString(),
    },
  });

  const eventsPerDay = React.useMemo(() => {
    const eventsPerDay: { [day: string]: EventInstanceExtendedFragment[] } = {};
    data?.list?.forEach((instance) => {
      const date = startOf(new Date(instance.since), 'day');
      const place = instance.event!.locationText;
      const key = date ? `${place} ${formatWeekDay(date)}` : place ?? '';
      eventsPerDay[key] = eventsPerDay[key] || [];
      eventsPerDay[key]!.push(instance);
    });
    return eventsPerDay;
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje lekce" startDate={startDate} onChange={setStartDate} />

      {!fetching && !data?.list?.length && (
        <div className="text-neutral-11">Žádné akce tento týden</div>
      )}

      <div className="flex flex-wrap flex-col gap-x-2">
        {Object.entries(eventsPerDay).map(([key, eventInstances]) => (
          <Card className="grid w-72 rounded-lg border-neutral-6 border">
            <h6>
              <div className="font-bold mb-1">{key.split(' ')[1]!}</div>
              <div className="text-sm text-neutral-11">{key.split(' ')[0]!}</div>
            </h6>
            {eventInstances.map((instance) => (
              <EventButton key={instance.id} instance={instance} />
            ))}
          </Card>
        ))}
      </div>

      {cohorts?.some((x) => x.sVisible) && (
        <h3 className="text-2xl tracking-wide mt-12 mb-4">Moje tréninková skupina</h3>
      )}
      {cohorts
        .filter((x) => x.sVisible)
        .map((cohort) => (
          <>
            <Card cohort={cohort}>
              <h3 className="text-2xl tracking-wide mb-4">{cohort.sName}</h3>
              <RichTextView value={cohort.sDescription} />
              <RichTextView value={cohort.internalInfo} />
            </Card>
          </>
        ))}
    </div>
  );
};
