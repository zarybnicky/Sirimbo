import { CalendarViews } from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import {
  useCalendarData,
  calendarTypes,
  calendarTypeOptions,
  type CalendarFilters,
} from '@/calendar/useCalendarData';
import type { EventRegistrationAttendanceFragment } from '@/graphql/Event';
import { compareText } from '@/lib/compare';
import { CompetitionEventContent } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
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
}: {
  personIds?: string[];
  cohortId?: string;
}) {
  const [date, setDate] = React.useState(() => new Date());
  const [filters, setFilters] = React.useState(calendarTypes);
  const list = React.useRef<HTMLDivElement>(null);
  const todayMarker = React.useRef<HTMLElement>(null);
  const scrollTo = React.useRef<'today' | 'top' | null>('today');
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
    if (fetching || error || !scrollTo.current || !list.current) return;
    const target = scrollTo.current === 'today' ? todayMarker.current : list.current;
    target?.scrollIntoView({ block: 'start', behavior: 'instant' });
    scrollTo.current = null;
  }, [date, fetching, error]);

  return (
    <>
      <div className="mb-3 flex flex-wrap items-center justify-between gap-3">
        <h4 className="text-xl tracking-tight">{capitalize(monthView.label(range))}</h4>
        <DateNavigator
          date={date}
          view={monthView}
          setDate={(next) => {
            scrollTo.current = 'top';
            setDate(next);
          }}
          onToday={() => {
            scrollTo.current = 'today';
            setDate(new Date());
          }}
        />
        <FacetFilter
          label="Typy"
          value={filters}
          defaultValue={calendarTypes}
          options={calendarTypeOptions}
          onChange={setFilters}
        />
      </div>
      <FormError error={error} />
      <div className="text-sm text-neutral-9" role="status">
        {fetching
          ? 'Načítám...'
          : !error && !days.some(([, items]) => items.length)
            ? 'Žádná aktivita v tomto měsíci.'
            : null}
      </div>
      <div ref={list} className="scroll-mt-20">
        {days.map(([day, items]) => {
          const events = items.filter((item) => item.kind === 'event');
          const birthdays = items.filter((item) => item.kind === 'birthday');
          return (
            <section
              key={day}
              ref={day === today ? todayMarker : undefined}
              className="scroll-mt-20"
            >
              {items.length > 0 && (
                <div className="mb-4 flex flex-col gap-2">
                  <h4 className="my-2 text-xl tracking-tight">
                    {capitalize(longDayFormatter.format(new Date(`${day}T00:00:00`)))}
                  </h4>
                  {birthdays.length > 0 && (
                    <div className="flex items-center gap-2 text-sm">
                      <Cake
                        className="size-4 shrink-0 text-accent-11"
                        aria-label="Narozeniny"
                      />
                      <LocalPersonLinks people={birthdays.map(({ person }) => person)} />
                    </div>
                  )}
                  {items.filter((item) => item.kind === 'competition').map(({ id, items, officials }) => (
                    <div
                      key={id}
                      className={cardCls({
                        className: 'rounded-lg border-green-7 bg-green-2 p-3',
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
                          viewer="auto"
                          suffix={
                            <AttendanceSummary
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
  items = [],
  compact,
}: {
  items:
    Pick<EventRegistrationAttendanceFragment, 'id' | 'status' | 'person'>[] | undefined;
  compact: boolean;
}) {
  if (compact) {
    const AttendedIcon = attendanceIcons.ATTENDED;
    const NotExcusedIcon = attendanceIcons.NOT_EXCUSED;
    return (
      <div className="inline-flex items-center gap-1 whitespace-nowrap text-neutral-11">
        <AttendedIcon className="size-4 text-green-11" aria-label="Přítomni" />
        <span>{items.filter(({ status }) => status === 'ATTENDED').length}</span>
        <NotExcusedIcon className="size-4 text-danger-11" aria-label="Nepřítomni" />
        <span>{items.filter(({ status }) => status === 'NOT_EXCUSED').length}</span>
        <span>/ {items.length}</span>
      </div>
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
