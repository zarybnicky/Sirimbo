import { CalendarViews } from '@/calendar/CalendarViews';
import type { CalendarCompetitionEvent } from '@/calendar/types';
import {
  useCalendarData,
  calendarTypes,
  type CalendarFilters,
} from '@/calendar/useCalendarData';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { CompetitionEventContent } from '@/ui/Competitions';
import { useAuth } from '@/lib/auth';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { capitalize, fullDateFormatter, longDayFormatter } from '@/ui/format';
import { startOf } from 'date-arithmetic';
import * as React from 'react';
import { buttonCls, cardCls } from '@/ui/style';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { Plus } from 'lucide-react';

const myEventsFilters: CalendarFilters = {
  free: false,
  participants: { onlyMine: true },
  trainers: [],
  location: [],
  types: calendarTypes,
  relevantOnly: false,
  columns: null,
};

export const MyEventsList = React.memo(function MyEventsList() {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const { events, competitions, fetching } = useCalendarData({
    view: CalendarViews.week,
    date: startDate,
    filters: myEventsFilters,
    withFacets: false,
  });
  const eventsByDay = React.useMemo(() => {
    const days = new Map<
      string,
      { events: EventWithTrainerFragment[]; competitions: CalendarCompetitionEvent[] }
    >();
    for (const event of events) {
      const date = startOf(new Date(event.since), 'day').toISOString();
      const day = days.get(date);
      if (day) day.events.push(event);
      else days.set(date, { events: [event], competitions: [] });
    }
    for (const competition of competitions) {
      const date = startOf(competition.start, 'day').toISOString();
      const day = days.get(date);
      if (day) day.competitions.push(competition);
      else days.set(date, { events: [], competitions: [competition] });
    }
    return [...days].toSorted(([a], [b]) => a.localeCompare(b));
  }, [events, competitions]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />

      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!fetching && eventsByDay.length === 0 ? 'Žádné nadcházející akce' : ''}
      </div>

      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsByDay.map(([date, { events, competitions }]) => (
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
            {competitions.map(({ id, items, officials }) => (
              <div
                key={id}
                className="mx-2 mb-3 rounded-lg border border-green-7 bg-green-2 p-3"
              >
                <CompetitionEventContent entries={items} officials={officials} />
              </div>
            ))}
            {events.map((event, i) => {
              const location = event.location?.name || event.locationText || '';
              const previous = i === 0 ? null : events.at(i - 1);
              const previousLocation = previous
                ? previous.location?.name || previous.locationText || ''
                : null;

              return (
                <React.Fragment key={event.id}>
                  {!!location && location !== previousLocation && (
                    <div className="ml-2.5 mt-1 text-sm text-accent-11">{location}</div>
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
