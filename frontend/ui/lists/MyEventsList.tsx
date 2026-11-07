import { CalendarViews } from '@/calendar/CalendarViews';
import {
  type CalendarFilters,
  calendarTypes,
  useCalendarData,
} from '@/calendar/useCalendarData';
import { CompetitionEventContent } from '@/ui/Competitions';
import { useAuth } from '@/lib/auth';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import { capitalize, longDayFormatter } from '@/ui/format';
import { startOf } from 'date-arithmetic';
import * as React from 'react';
import { buttonCls, cardCls } from '@/ui/style';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { EventForm } from '@/ui/event-form/EventForms';
import { Plus } from 'lucide-react';
import { FormError } from '@/ui/form';

const myEventsFilters: CalendarFilters = {
  groupBy: 'none',
  free: false,
  participants: { onlyMine: true },
  trainers: [],
  location: [],
  types: calendarTypes.filter((type) => type !== 'BIRTHDAY'),
  relevantOnly: false,
  columns: null,
};

export const MyEventsList = React.memo(function MyEventsList() {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));
  const { calendarEvents, fetching, error } = useCalendarData({
    view: CalendarViews.week,
    date: startDate,
    filters: myEventsFilters,
    withFacets: false,
    withOtherClubCompetitors: true,
  });
  const eventsByDay = React.useMemo(
    () => [...Map.groupBy(calendarEvents, (x) => +startOf(x.start, 'day'))].toSorted(
      ([a], [b]) => a - b,
    ),
    [calendarEvents],
  );

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje události" startDate={startDate} onChange={setStartDate} />
      {error && <FormError error={error} />}
      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!error && !fetching && eventsByDay.length === 0 ? 'Žádné akce tento týden' : ''}
      </div>
      <div className="flex flex-wrap flex-col gap-x-2">
        {eventsByDay.map(([date, items]) => {
          const events = items.filter((x) => x.kind === 'event').map((x) => x.instance);
          const comps = items.filter((x) => x.kind === 'competition');
          return (
            <div
              key={date}
              className={cardCls({ className: 'rounded-lg border-neutral-6 border px-1 py-3' })}
            >
              <div className="ml-3 mb-1 flex items-center gap-2 font-bold">
                {capitalize(longDayFormatter.format(new Date(date)))}
                {auth.isTrainer && (
                  <Dialog modal={false}>
                    <DialogTrigger.Plain
                      type="button"
                      className={buttonCls({ display: 'none', variant: 'none' })}
                    >
                      <Plus />
                    </DialogTrigger.Plain>
                    <DialogContent className="sm:max-w-xl">
                      <EventForm date={new Date(date)} events={events} />
                    </DialogContent>
                  </Dialog>
                )}
              </div>
              {comps.map(({ id, items, officials, clubEntries }) => (
                <div
                  key={id}
                  className="mx-2 mb-3 rounded-lg border border-green-7 bg-green-2 p-3"
                >
                  <CompetitionEventContent
                    entries={items}
                    officials={officials}
                    clubEntries={clubEntries}
                  />
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
          );
        })}
      </div>
    </div>
  );
});
