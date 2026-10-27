import { EventListDocument } from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { capitalize, fullDateFormatter, longDayFormatter } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';
import { buttonCls, cardCls } from '@/ui/style';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { Plus } from 'lucide-react';

export const MyEventsList = React.memo(function MyEventsList() {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data, fetching }] = useQuery({
    query: EventListDocument,
    variables: {
      condition: {
        overlaps: {
          start: startDate.toISOString(),
          end: add(startDate, 1, 'week').toISOString(),
        },
        scope: { mode: 'MINE' },
      },
      orderBy: ['SINCE_ASC'],
    },
  });

  const eventsByDay = React.useMemo(() => {
    const map = new Map<string, EventWithTrainerFragment[]>();
    for (const event of data?.eventsList ?? []) {
      const date = startOf(new Date(event.since), 'day').toISOString();
      const items = map.get(date);
      if (items) items.push(event);
      else map.set(date, [event]);
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
        {!fetching && !data?.eventsList?.length ? 'Žádné nadcházející akce' : ''}
      </div>

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsByDay.map(([date, events]) => (
          <div
            key={date}
            className={cardCls({
              className: 'rounded-lg border-neutral-6 border px-1 py-3',
            })}
          >
            <div className="ml-3 mb-1 flex items-center gap-2 font-bold">
              {capitalize(longDayFormatter.format(new Date(date)))}
              {auth.isTrainer && (
                <Dialog modal={false}>
                  <DialogTrigger.Plain
                    type="button"
                    aria-label={`Přidat událost na ${fullDateFormatter.format(new Date(date))}`}
                    className={buttonCls({ display: 'none', variant: 'none' })}
                  >
                    <Plus />
                  </DialogTrigger.Plain>
                  <DialogContent className="sm:max-w-xl">
                    <CreateEventForm date={new Date(date)} events={events} />
                  </DialogContent>
                </Dialog>
              )}
            </div>
            {events.map((event, i) => {
              const location = event.location?.name || event.locationText || '';
              const previous = i === 0 ? null : events.at(i - 1);
              const previousLocation = previous
                ? previous.location?.name || previous.locationText || ''
                : null;

              return (
                <React.Fragment key={event.id}>
                  {!!location && location !== previousLocation && (
                    <div className="ml-2.5 mt-1 text-sm text-accent-11">
                      {location}
                    </div>
                  )}
                  <EventButton event={event} viewer="auto" attendance="inline" />
                </React.Fragment>
              );
            })}
          </div>
        ))}
      </div>
    </div>
  );
});
