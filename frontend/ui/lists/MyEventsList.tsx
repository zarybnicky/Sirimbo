import { CalendarViews } from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import {
  type CalendarFilters,
  calendarTypes,
  useCalendarData,
} from '@/calendar/useCalendarData';
import { CompetitionEventCard } from '@/ui/Competitions';
import { useAuth } from '@/lib/auth';
import { EventButton } from '@/ui/EventButton';
import { capitalize, longDayFormatter } from '@/ui/format';
import { startOf } from 'date-arithmetic';
import * as React from 'react';
import { buttonCls, cardCls, typographyCls } from '@/ui/style';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { EventForm } from '@/ui/event-form/EventForms';
import { CalendarDays, List, Plus, Search } from 'lucide-react';
import { FormError } from '@/ui/form';
import Link from 'next/link';
import { format } from 'date-fns/format';
import { getWeek } from 'date-fns/getWeek';
import { useAtom } from 'jotai';
import { savedSearchAtom } from '@/calendar/state';
import { EventSavedSearchPicker } from '@/ui/EventSavedSearchPicker';

const myEventsFilters: CalendarFilters = {
  groupBy: 'none',
  canRegister: false,
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
  const [savedSearch, setSavedSearch] = useAtom(savedSearchAtom);

  const { calendarEvents, savedSearches, fetching, error } = useCalendarData({
    view: CalendarViews.week,
    date: startDate,
    filters: myEventsFilters,
    savedSearch,
    withSavedSearches: auth.isSystemAdmin,
    withFacets: false,
    withCompetitions:
      !savedSearch?.condition?.canRegister &&
      (savedSearch?.condition?.withCompetitions ?? true),
    withBirthdays:
      !!savedSearch?.condition?.withBirthdays && !savedSearch.condition.canRegister,
    withOtherClubCompetitors: true,
  });

  const eventsByDay = React.useMemo(
    () =>
      [...Map.groupBy(calendarEvents, (x) => +startOf(x.start, 'day'))].toSorted(
        ([a], [b]) => a - b,
      ),
    [calendarEvents],
  );

  return (
    <div className="flex flex-col">
      <div>
        <div className="flex flex-wrap items-center justify-between gap-2">
          <h4 className={typographyCls({ variant: 'smallHeading', className: 'grow' })}>
            {auth.isSystemAdmin ? (
              <EventSavedSearchPicker
                savedSearches={savedSearches}
                value={savedSearch?.id ?? null}
                onChange={(id) =>
                  setSavedSearch(savedSearches.find((x) => x.id === id) ?? null)
                }
              />
            ) : 'Moje události'}
          </h4>
          <DateNavigator
            variant="subtle"
            date={startDate}
            view={CalendarViews.week}
            setDate={(date) => setStartDate(startOf(date, 'week', 1))}
          />
        </div>
        <div className="mb-2 flex items-center text-neutral-11 text-sm">
          {CalendarViews.week.label(CalendarViews.week.range(startDate))}
          {` (${getWeek(startDate, { weekStartsOn: 1 })}. týden)`}
        </div>
      </div>
      <FormError error={error} />
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
              className={cardCls({
                className: 'rounded-lg border-neutral-6 border p-1',
              })}
            >
              <div className="ml-2 flex justify-between flex-wrap items-center gap-2">
                <h4 className="md:text-lg tracking-tight">
                  {capitalize(longDayFormatter.format(new Date(date)))}
                </h4>
                <div className="flex gap-1">
                  {auth.isTrainer && (
                    <Dialog modal={false}>
                      <DialogTrigger.Plain
                        type="button"
                        className={buttonCls({ display: 'none', variant: 'none' })}
                      >
                        <Plus />
                      </DialogTrigger.Plain>
                      <DialogContent className="sm:max-w-xl">
                        <EventForm
                          date={new Date(date)}
                          after={events.at(-1)}
                          events={events}
                        />
                      </DialogContent>
                    </Dialog>
                  )}
                  <Link
                    href={`/rozpis?v=agenda&date=${format(date, 'yyyy-MM-dd')}`}
                    className={buttonCls({ display: 'none', variant: 'none' })}
                  >
                    <span className="relative inline-block size-5 align-middle">
                      <List className="size-4!" />
                      <Search className="absolute right-0 bottom-0 size-2.5! rounded-xs bg-neutral-1 ring-2 ring-neutral-1" />
                    </span>
                  </Link>
                  <Link
                    href={`/rozpis?v=week&date=${format(date, 'yyyy-MM-dd')}`}
                    className={buttonCls({ display: 'none', variant: 'none' })}
                  >
                    <span className="relative inline-block size-5 align-middle">
                      <CalendarDays className="size-4!" />
                      <Search className="absolute right-0 bottom-0 size-2.5! rounded-xs bg-neutral-1 ring-2 ring-neutral-1" />
                    </span>
                  </Link>
                </div>
              </div>

              {comps.map((x) => (
                <CompetitionEventCard key={x.id} {...x} className="mx-2 mb-3" />
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
