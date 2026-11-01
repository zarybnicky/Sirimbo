import { CalendarViews } from '@/calendar/CalendarViews';
import {
  useCalendarData,
  calendarTypes,
  type CalendarFilters,
} from '@/calendar/useCalendarData';
import type { CalendarCompetitionEvent } from '@/calendar/types';
import { CompetitionEventContent, type CompetitionEntry } from '@/ui/Competitions';
import { useAuth } from '@/lib/auth';
import { EventButton } from '@/ui/EventButton';
import { WeekPicker } from '@/ui/WeekPicker';
import {
  capitalize,
  formatCount,
  fullDateFormatter,
  longDayFormatter,
} from '@/ui/format';
import { startOf } from 'date-arithmetic';
import * as React from 'react';
import { buttonCls, cardCls } from '@/ui/style';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { Plus } from 'lucide-react';
import * as Collapsible from '@radix-ui/react-collapsible';
import { FormError } from '@/ui/form';

// TODO: Redesign club competition summaries and expansion before enabling.
const showClubCompetitions = false;

const myEventsFilters: CalendarFilters = {
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
  const { calendarEvents, clubCompetitions, fetching, error } = useCalendarData({
    view: CalendarViews.week,
    date: startDate,
    filters: myEventsFilters,
    withFacets: false,
    withClubCompetitions: showClubCompetitions && auth.isMember,
  });
  const eventsByDay = React.useMemo(() => {
    const comps = new Map<
      string,
      CalendarCompetitionEvent & { club: readonly CompetitionEntry[] }
    >(
      calendarEvents
        .filter((x) => x.kind === 'competition')
        .map((x) => [x.id, { ...x, club: [] }]),
    );
    for (const comp of showClubCompetitions ? clubCompetitions : []) {
      const mine = comps.get(comp.id);
      const ids = new Set(mine?.items.map((x) => `${x.competitionId}:${x.competitorId}`));
      const club = comp.items.filter(
        (x) =>
          x.competition.status !== 'CANCELLED' &&
          (!x.cancelled || x.ranking !== undefined) &&
          !ids.has(`${x.competitionId}:${x.competitorId}`),
      );
      if (club.length > 0)
        comps.set(comp.id, { ...(mine ?? { ...comp, items: [] }), club });
    }
    const events = [
      ...calendarEvents.filter((x) => x.kind !== 'competition'),
      ...comps.values(),
    ];
    return [...Map.groupBy(events, (x) => +startOf(x.start, 'day'))].toSorted(
      ([a], [b]) => a - b,
    );
  }, [calendarEvents, clubCompetitions]);

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
          const own =
            events.length > 0 ||
            comps.some((x) => x.items.length > 0 || x.officials?.length);
          return (
            <div
              key={date}
              className={
                own
                  ? cardCls({ className: 'rounded-lg border-neutral-6 border px-1 py-3' })
                  : 'px-1 py-1.5'
              }
            >
              <div
                className={`ml-3 mb-1 flex items-center gap-2 ${own ? 'font-bold' : 'text-sm text-neutral-11'}`}
              >
                {capitalize(longDayFormatter.format(new Date(date)))}
                {own && auth.isTrainer && (
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
              {comps.map(({ id, title, items, officials, club }) => {
                const mine = items.length > 0 || !!officials?.length;
                const event = club[0]?.competition.event;
                return (
                  <div
                    key={id}
                    className={
                      mine
                        ? 'mx-2 mb-3 rounded-lg border border-green-7 bg-green-2 p-3'
                        : 'mx-3 mb-1'
                    }
                  >
                    {mine && (
                      <CompetitionEventContent
                        entries={items}
                        officials={officials}
                        clubEntries={club}
                      />
                    )}
                    {!mine && club.length > 0 && (
                      <Collapsible.Root className="mt-1">
                        <Collapsible.Trigger asChild>
                          <button
                            type="button"
                            className="text-left text-sm text-accent-11 underline underline-offset-2 hover:text-accent-12 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
                          >
                            {title}
                          </button>
                        </Collapsible.Trigger>
                        <div className="text-xs text-neutral-11">
                          {(event?.location || event?.city) &&
                            `${event.location || event.city} · `}
                          {formatCount(new Set(club.map((x) => x.competitorId)).size, {
                            one: 'soutěžící',
                            few: 'soutěžící',
                            other: 'soutěžících',
                          })}
                        </div>
                        <Collapsible.Content className="mt-2 rounded-lg border border-green-7 bg-green-2 p-3">
                          <CompetitionEventContent entries={club} showHeader={false} />
                        </Collapsible.Content>
                      </Collapsible.Root>
                    )}
                  </div>
                );
              })}
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
