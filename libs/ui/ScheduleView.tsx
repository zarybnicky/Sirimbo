import * as React from 'react';
import { formatWeekDay } from '@app/ui/format-date';
import { WeekPicker } from '@app/ui/WeekPicker';
import { useQuery } from 'urql';
import { endOf, startOf } from 'date-arithmetic';
import { useAuth } from './use-auth';
import { Card } from '@app/ui/Card';
import { EventInstanceExtendedFragment, EventInstanceRangeDocument } from '@app/graphql/Event';
import { EventButton } from './EventButton';

export function ScheduleView() {
  const { user } = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      range: {
        start: {
          inclusive: true,
          value: startDate.toISOString(),
        },
        end: {
          inclusive: true,
          value: endOf(startDate, 'week', 1).toISOString(),
        },
      },
    },
  });

  const dataByDay = React.useMemo(() => {
    const obj: { [date: string]: EventInstanceExtendedFragment[] } = {};
    data?.list?.forEach((item) => {
      const arr = obj[item.range.start!.value] || [];
      arr.push(item);
      obj[item.range.start!.value] = arr;
    });
    return obj;
  }, [data]);

  return (
    <div className={user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] py-4 mb-8'}>
      <WeekPicker title="Tréninky" startDate={startDate} onChange={setStartDate} />

      {!data?.list?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {Object.entries(dataByDay).map(([date, items], i) => (
        <React.Fragment key={i}>
          <div className="text-2xl tracking-wide mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-red-400">
            {items.map((item) => (
              <Card key={item.id} className="group min-w-[200px] w-72 rounded-lg border-accent-7 border">
                <div className="ml-3 mb-0.5">
                  <div className="text-sm text-accent-11">{item.event!.locationText}</div>
                  <div className="text-xl">
                    {item.event!.eventTrainersList.map(x => `${x.person!.firstName} ${x.person!.lastName}`).join(', ')}
                  </div>
                </div>
                <EventButton key={i} instance={item} />
              </Card>
            ))}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}
