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
import { CompetitionEventContent } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
import { AttendanceStatsLink } from '@/ui/AttendanceStats';
import { attendanceIcons } from '@/ui/EventAttendance';
import { FacetFilter } from '@/ui/FacetFilter';
import { LocalPersonLinks } from '@/ui/federation-links';
import { capitalize, longDayFormatter } from '@/ui/format';
import { FormError } from '@/ui/form';
import { cardCls } from '@/ui/style';
import { endOf, startOf } from 'date-arithmetic';
import { format } from 'date-fns';
import { Cake } from 'lucide-react';
import * as React from 'react';

const monthView = {
  ...CalendarViews.month,
  range: (date: Date) => ({
    since: startOf(date, 'month'),
    until: endOf(date, 'month'),
  }),
};
const calendarFilters: CalendarFilters = {
  participants: {},
  trainers: [],
  location: [],
  types: calendarTypes,
  relevantOnly: false,
  free: false,
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
        <h4 className="text-lg tracking-tight flex-1">{capitalize(monthView.label(range))}</h4>
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
          : !error && !days.some(([, items]) => items.length)
            ? 'Žádná aktivita v tomto měsíci.'
            : null}
      </div>
      <div className="scroll-mt-20">
        {days.map(([day, items]) => {
          const events = items.filter((x) => x.kind === 'event');
          const birthdays = items.filter((x) => x.kind === 'birthday');
          return (
            <section
              key={day}
              ref={day === today ? todayMarker : undefined}
              className="scroll-mt-20"
            >
              {items.length > 0 && (
                <div className="mb-2 flex flex-col gap-1">
                  <h4 className="text-lg tracking-tight">
                    {capitalize(longDayFormatter.format(new Date(`${day}T00:00:00`)))}
                  </h4>
                  {birthdays.length > 0 && (
                    <div className="flex items-center gap-2 text-sm">
                      <Cake className="size-4 shrink-0 text-accent-11" />
                      <LocalPersonLinks people={birthdays.map(({ person }) => person)} />
                    </div>
                  )}
                  {items
                    .filter((x) => x.kind === 'competition')
                    .map(({ id, items, officials }) => (
                      <div
                        key={id}
                        className={cardCls({
                          className: 'rounded-lg border-green-7 bg-green-2 p-3 mb-0',
                        })}
                      >
                        <CompetitionEventContent entries={items} officials={officials} />
                      </div>
                    ))}
                  {events.length > 0 && (
                    <div
                      className={cardCls({
                        className: 'rounded-lg border-neutral-6 border p-0',
                      })}
                    >
                      {events.map(({ id, instance }) => (
                        <EventButton
                          key={id}
                          event={instance}
                          viewer={viewerId ? 'auto' : 'neutral'}
                          viewerId={viewerId}
                          suffix={
                            <AttendanceSummary
                              eventId={instance.id}
                              items={instance.attendance}
                              compact={!!cohortId}
                            />
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
}: {
  items:
    Pick<EventRegistrationAttendanceFragment, 'id' | 'status' | 'person'>[] | undefined;
  compact: boolean;
  eventId: string;
}) {
  if (compact) {
    return (
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
