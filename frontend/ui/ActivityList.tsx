import { CalendarViews } from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import {
  type CalendarFilters,
  calendarTypeOptions,
  calendarTypes,
  useCalendarData,
} from '@/calendar/useCalendarData';
import type { EventRegistrationAttendanceFragment } from '@/graphql/Event';
import { compareText } from '@/lib/compare';
import { CompetitionEventCard } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
import { AttendanceStatsLink } from '@/ui/AttendanceStats';
import { attendanceIcons } from '@/ui/EventAttendance';
import { FacetFilter } from '@/ui/FacetFilter';
import { LocalPersonLinks } from '@/ui/federation-links';
import { capitalize, longDayFormatter } from '@/ui/format';
import { FormError } from '@/ui/form';
import { buttonCls, cardCls } from '@/ui/style';
import { endOf, startOf } from 'date-arithmetic';
import { format } from 'date-fns';
import { Cake, CalendarDays, List, Plus, Search } from 'lucide-react';
import * as React from 'react';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { EventForm } from './event-form/EventForms';
import Link from 'next/link';
import { useAuth } from '@/lib/auth';

const monthView = {
  ...CalendarViews.month,
  range: (date: Date) => ({
    since: startOf(date, 'month'),
    until: endOf(date, 'month'),
  }),
};
const calendarFilters: CalendarFilters = {
  groupBy: 'none',
  participants: {},
  trainers: [],
  location: [],
  types: calendarTypes,
  relevantOnly: false,
  canRegister: false,
  columns: null,
};

export const ActivityList = React.memo(function ActivityList({
  personIds,
  cohortId,
  viewerId,
}: {
  personIds?: string[];
  cohortId?: string;
  viewerId?: string;
}) {
  const auth = useAuth();
  const [date, setDate] = React.useState(() => new Date());
  const [filters, setFilters] = React.useState(calendarTypes);
  const todayMarker = React.useRef<HTMLElement>(null);
  const scrollToToday = React.useRef(false);
  const today = format(new Date(), 'yyyy-MM-dd');
  const { range, calendarEvents, fetching, error } = useCalendarData({
    view: monthView,
    date,
    filters: {
      ...calendarFilters,
      participants: { personIds, cohortId },
      types: filters,
    },
    withFacets: false,
    withAttendance: true,
    withCompetitions: filters.includes('COMPETITION'),
    withBirthdays: filters.includes('BIRTHDAY'),
    withOtherClubCompetitors: true,
  });

  const days = React.useMemo(() => {
    const grouped = Map.groupBy(calendarEvents, (event) =>
      format(event.start, 'yyyy-MM-dd'),
    );
    if (
      today >= format(range.since, 'yyyy-MM-dd') &&
      today <= format(range.until, 'yyyy-MM-dd') &&
      !grouped.has(today)
    ) {
      grouped.set(today, []);
    }
    return [...grouped].toSorted(([a], [b]) => a.localeCompare(b));
  }, [calendarEvents, range, today]);

  React.useEffect(() => {
    if (fetching || error || !scrollToToday.current) return;
    todayMarker.current?.scrollIntoView({ block: 'start', behavior: 'auto' });
    scrollToToday.current = false;
  }, [date, fetching, error]);

  return (
    <>
      <div className="mb-3 flex flex-wrap items-center justify-between gap-3">
        <DateNavigator
          date={date}
          view={monthView}
          setDate={(next) => {
            scrollToToday.current = false;
            setDate(next);
          }}
          onToday={() => {
            scrollToToday.current = true;
            setDate(new Date());
          }}
        />
        <h4 className="text-lg tracking-tight flex-1">
          {capitalize(monthView.label(range))}
        </h4>
        <FacetFilter
          label="Typy"
          value={filters}
          defaultValue={calendarTypes}
          options={calendarTypeOptions}
          onChange={setFilters}
        />
      </div>
      <FormError error={error} />
      <div className="text-sm text-neutral-9">
        {fetching
          ? 'Načítám...'
          : !error && !days.some(([, xs]) => xs.length)
            ? 'Žádná aktivita v tomto měsíci.'
            : null}
      </div>
      <div className="scroll-mt-20">
        {days.map(([day, items]) => {
          const events = items.filter((x) => x.kind === 'event').map((x) => x.instance);
          const birthdays = items.filter((x) => x.kind === 'birthday');
          return (
            <section
              key={day}
              ref={day === today ? todayMarker : undefined}
              className="scroll-mt-20"
            >
              {items.length > 0 && (
                <div className="mb-2 flex flex-col gap-1">
                  <div className="flex justify-between flex-wrap items-center gap-2">
                    <h4 className="md:text-lg tracking-tight">
                      {capitalize(longDayFormatter.format(new Date(`${day}T00:00:00`)))}
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

                  {birthdays.length > 0 && (
                    <div className="flex items-center gap-2 text-sm">
                      <Cake className="size-4 shrink-0 text-accent-11" />
                      <LocalPersonLinks people={birthdays.map(({ person }) => person)} />
                    </div>
                  )}
                  {items
                    .filter((x) => x.kind === 'competition')
                    .map((x) => (
                      <CompetitionEventCard key={x.id} {...x} className="mb-0" />
                    ))}
                  {events.length > 0 && (
                    <div
                      className={cardCls({
                        className: 'rounded-lg border-neutral-6 border p-0',
                      })}
                    >
                      {events.map((clubEvent) => (
                        <EventButton
                          key={clubEvent.id}
                          event={clubEvent}
                          viewer={viewerId ? 'auto' : 'neutral'}
                          viewerId={viewerId}
                          suffix={
                            clubEvent.type !== 'LESSON' && (
                              <AttendanceSummary
                                eventId={clubEvent.id}
                                items={clubEvent.attendance}
                                compact={!!cohortId}
                                future={new Date(clubEvent.since).getTime() > Date.now()}
                              />
                            )
                          }
                        />
                      ))}
                    </div>
                  )}
                </div>
              )}
            </section>
          );
        })}
      </div>
    </>
  );
});

function AttendanceSummary({
  eventId,
  items = [],
  compact,
  future,
}: {
  items:
    Pick<EventRegistrationAttendanceFragment, 'id' | 'status' | 'person'>[] | undefined;
  compact: boolean;
  future: boolean;
  eventId: string;
}) {
  if (compact) {
    return future ? null : (
      <AttendanceStatsLink
        eventId={eventId}
        stats={{
          total: items.length,
          attended: items.filter(({ status }) => status === 'ATTENDED').length,
          notExcused: items.filter(({ status }) => status === 'NOT_EXCUSED').length,
          unknown: items.filter(({ status }) => !status || status === 'UNKNOWN').length,
        }}
      />
    );
  }
  return (
    <div className="inline-flex flex-wrap items-center justify-end gap-3 text-sm">
      {items
        .filter(({ status }) => !future || (status !== null && status !== 'UNKNOWN'))
        .toSorted((a, b) => compareText(a.person?.name ?? '', b.person?.name ?? ''))
        .map(({ id, person, status }) => {
          const Icon = status ? attendanceIcons[status] : null;
          return (
            <span
              key={id}
              className="inline-flex items-center gap-1 whitespace-nowrap text-neutral-11"
            >
              {person?.firstName ?? person?.name}
              {Icon && <Icon className="size-4 shrink-0" />}
            </span>
          );
        })}
    </div>
  );
}
