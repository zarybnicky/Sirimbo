import {
  ActivityTimelineDocument,
  type ActivityTimelineItemFragment,
  type ActivityTimelineItem_ActivityEventAttendance_Fragment,
  type ActivityTimelineItem_ActivityCompetitionBrief_Fragment,
  type ActivityTimelineItem_ActivityCompetitionResult_Fragment,
} from '@/graphql/ActivityTimeline';
import type { ActivityTimelineKind, AttendanceType, EventType } from '@/graphql';
import { cn } from '@/lib/cn';
import { CompetitionEventContent, type CompetitionEntry } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
import { attendanceIcons } from '@/ui/InstanceAttendanceView';
import { formatWeekDay } from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { add, subtract } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';

type TimelineMode = 'future' | 'past';
type TimelineFilter = EventType | 'COMPETITION';
type TimelineItem = NonNullable<ActivityTimelineItemFragment>;
type EventAttendanceItem = ActivityTimelineItem_ActivityEventAttendance_Fragment;
type CompetitionItem =
  | ActivityTimelineItem_ActivityCompetitionBrief_Fragment
  | ActivityTimelineItem_ActivityCompetitionResult_Fragment;

const RANGE_WEEKS = 8;
const FILTERS = [
  ['GROUP', 'Společná'],
  ['LESSON', 'Lekce'],
  ['CAMP', 'Soustředění'],
  ['RESERVATION', 'Nabídka'],
  ['HOLIDAY', 'Prázdniny'],
  ['COMPETITION', 'Soutěže'],
] as const satisfies ReadonlyArray<readonly [TimelineFilter, string]>;

function rangeFor(mode: TimelineMode, pages: number) {
  const now = new Date();
  const weeks = RANGE_WEEKS * pages;
  return mode === 'future'
    ? { since: now, until: add(now, weeks, 'week') }
    : { since: subtract(now, weeks, 'week'), until: now };
}

function competitionEventKey(item: CompetitionItem) {
  return [
    item.competitionEventId ?? '',
    item.competitionDate ?? '',
    item.competitionEventName ?? '',
    item.competitionEventLocation ?? '',
  ].join(':');
}

function toCompetitionEntry(item: CompetitionItem): CompetitionEntry {
  const base = {
    eventId: item.competitionEventId,
    eventName: item.competitionEventName,
    eventLocation: item.competitionEventLocation,
    competitionId: item.competitionId,
    competitionDate: item.competitionDate,
    competitionType: item.competitionType,
    competitorId: item.competitorName ?? item.personId,
    competitorName: item.competitorName,
    personId: item.personId,
    personName: item.person?.name ?? null,
    category: item.category,
  };

  return item.__typename === 'ActivityCompetitionResult'
    ? {
        ...base,
        kind: 'report',
        participants: item.participants,
        ranking: item.ranking,
        rankingTo: item.rankingTo,
        pointGain: item.pointGain,
        isFinal: item.isFinal,
      }
    : {
        ...base,
        kind: 'brief',
        checkInEnd: item.checkInEnd,
      };
}

export function ActivityTimeline({
  personIds,
  cohortId,
}: {
  personIds?: string[];
  cohortId?: string;
}) {
  const [mode, setMode] = React.useState<TimelineMode>('future');
  const [pages, setPages] = React.useState(1);
  const [filters, setFilters] = React.useState<TimelineFilter[]>(() =>
    FILTERS.map(([value]) => value).filter((x) => (cohortId ? x !== 'LESSON' : true)),
  );
  const scopeKey = `${cohortId ?? ''}:${personIds?.join(',') ?? ''}`;
  const range = React.useMemo(() => rangeFor(mode, pages), [mode, pages]);
  const hasScope = Boolean(cohortId || personIds?.length);
  const eventTypes = filters.filter(
    (value): value is EventType => value !== 'COMPETITION',
  );
  const kinds: ActivityTimelineKind[] = eventTypes.length > 0 ? ['EVENT_ATTENDANCE'] : [];
  if (filters.includes('COMPETITION')) {
    kinds.push('COMPETITION_BRIEF', 'COMPETITION_RESULT');
  }

  React.useEffect(() => setPages(1), [scopeKey]);

  const toggleFilter = (filter: TimelineFilter) => {
    setPages(1);
    setFilters((value) =>
      value.includes(filter)
        ? value.filter((item) => item !== filter)
        : [...value, filter],
    );
  };

  const [{ data, fetching, error }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: range.since.toISOString(),
      until: range.until.toISOString(),
      personIds,
      cohortId,
      kinds,
      eventTypes,
    },
    pause: !hasScope,
    requestPolicy: 'cache-and-network',
  });

  const days = React.useMemo(() => {
    const direction = mode === 'future' ? 1 : -1;
    const grouped = new Map<string, TimelineItem[]>();
    for (const item of (data?.activityTimelineList ?? [])
      .filter(Boolean)
      .toSorted((a, b) => {
        const byTime = (a.sortAt ?? '').localeCompare(b.sortAt ?? '') * direction;
        return byTime || a.id.localeCompare(b.id);
      })) {
      const key = (item.sortAt ?? item.activityDate ?? '').slice(0, 10);
      const group = grouped.get(key);
      if (group) group.push(item);
      else grouped.set(key, [item]);
    }
    return [...grouped.entries()];
  }, [data?.activityTimelineList, mode]);

  return (
    <section className="flex flex-col">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <div className="inline-flex rounded-xl shadow-md [&_button]:rounded-none [&_button]:shadow-none [&_button:first-child]:rounded-l-xl [&_button:last-child]:rounded-r-xl">
          {[
            ['future', 'Budoucí'],
            ['past', 'Minulé'],
          ].map(([key, label]) => (
            <button
              key={key}
              type="button"
              aria-pressed={mode === key}
              className={buttonCls({
                variant: mode === key ? 'primary' : 'outline',
                size: 'sm',
              })}
              onClick={() => {
                setMode(key as TimelineMode);
                setPages(1);
              }}
            >
              {label}
            </button>
          ))}
        </div>

        <div className="mb-2 mt-1 flex flex-wrap gap-1">
          {FILTERS.map(([filter, label]) => (
            <button
              key={filter}
              type="button"
              aria-pressed={filters.includes(filter)}
              className={buttonCls({
                variant: filters.includes(filter) ? 'outline' : 'none',
                size: 'xs',
                className: cn(
                  !filters.includes(filter) &&
                    'border border-neutral-6 bg-neutral-1 text-neutral-11 hover:bg-neutral-3',
                ),
              })}
              onClick={() => toggleFilter(filter)}
            >
              {label}
            </button>
          ))}
        </div>
      </div>

      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!fetching && error ? `Nepodařilo se načíst aktivitu: ${error.message}` : null}
        {!fetching && !error && days.length === 0
          ? mode === 'future'
            ? 'Žádná nadcházející aktivita.'
            : 'Žádná minulá aktivita.'
          : null}
      </div>

      <div className="flex flex-col gap-3">
        {days.map(([date, dayItems]) => (
          <TimelineDay
            key={date}
            date={date}
            items={dayItems}
            mode={mode}
            isCohort={Boolean(cohortId)}
          />
        ))}
      </div>

      <div className="mt-3 flex justify-center">
        <button
          type="button"
          className={buttonCls({ variant: 'outline', size: 'sm' })}
          onClick={() => setPages((value) => value + 1)}
        >
          {mode === 'future' ? 'Načíst další...' : 'Načíst starší...'}
        </button>
      </div>
    </section>
  );
}

function TimelineDay({
  date,
  items,
  mode,
  isCohort,
}: {
  date: string;
  items: TimelineItem[];
  mode: TimelineMode;
  isCohort: boolean;
}) {
  const eventGroups = new Map<string, EventAttendanceItem[]>();
  const competitionGroups = new Map<string, CompetitionItem[]>();

  for (const item of items) {
    if (item.__typename === 'ActivityEventAttendance' && item.eventInstance) {
      const key = item.eventInstance.id;
      eventGroups.set(key, [...(eventGroups.get(key) ?? []), item]);
    } else if (item.__typename !== 'ActivityEventAttendance') {
      const key = competitionEventKey(item);
      competitionGroups.set(key, [...(competitionGroups.get(key) ?? []), item]);
    }
  }

  const eventRows = [...eventGroups.values()].toSorted((a, b) =>
    (a[0]?.eventInstance?.since ?? '').localeCompare(b[0]?.eventInstance?.since ?? ''),
  );
  const competitionCards = [...competitionGroups.entries()].map(([key, groupItems]) => ({
    key,
    items: groupItems,
  }));

  return (
    <section>
      <h4 className="mb-2 mt-5 text-2xl tracking-wide first:mt-3">
        {date
          ? formatWeekDay(new Date(`${date}T00:00:00`))
          : mode === 'future'
            ? 'Budoucí'
            : 'Minulé'}
      </h4>
      <div className="flex flex-col gap-2">
        {competitionCards.length > 0 ? (
          <div className="flex flex-wrap justify-start gap-2 opacity-90">
            {competitionCards.map(({ key, items }) => (
              <CompetitionTimelineCard key={`competition:${key}`} items={items} />
            ))}
          </div>
        ) : null}
        {eventRows.length > 0 ? (
          <div
            className={cardCls({ className: 'rounded-lg border-neutral-6 border p-0' })}
          >
            {eventRows.map((rowItems) => (
              <TimelineEventButton
                mode={mode}
                key={rowItems[0]!.eventInstance!.id}
                items={rowItems}
                isCohort={isCohort}
              />
            ))}
          </div>
        ) : null}
      </div>
    </section>
  );
}

function TimelineEventButton({
  isCohort,
  items,
  mode,
}: {
  mode: TimelineMode;
  isCohort: boolean;
  items: EventAttendanceItem[];
}) {
  const instance = items[0]?.eventInstance;
  const event = instance?.event;

  if (!instance || !event) return null;

  return (
    <EventButton
      event={event}
      instance={instance}
      viewer="auto"
      suffix={
        (mode === 'past' || isCohort) && (instance.type !== 'LESSON' || isCohort) ? (
          <AttendanceSummary
            compact={isCohort && instance.type !== 'LESSON'}
            items={items}
          />
        ) : undefined
      }
    />
  );
}

function attendanceCounts(items: EventAttendanceItem[]) {
  const counts: Record<AttendanceType, number> = {
    ATTENDED: 0,
    CANCELLED: 0,
    NOT_EXCUSED: 0,
    UNKNOWN: 0,
  };

  for (const item of items) {
    const status = item.eventAttendance?.status;
    if (status) counts[status] += 1;
  }

  return counts;
}

function AttendanceSummary({
  compact,
  items,
}: {
  compact: boolean;
  items: EventAttendanceItem[];
}) {
  if (compact) {
    const counts = attendanceCounts(items);
    const AttendedIcon = attendanceIcons.ATTENDED;
    const NotExcusedIcon = attendanceIcons.NOT_EXCUSED;

    return (
      <div className="inline-flex items-center gap-1 whitespace-nowrap text-neutral-11">
        <AttendedIcon className="size-4 text-green-11" />
        <span>{counts.ATTENDED}</span>
        <NotExcusedIcon className="size-4 text-[#b42346] dark:text-[#ffb4c2]" />
        <span>{counts.NOT_EXCUSED}</span>
        <span>/ {items.length}</span>
      </div>
    );
  }

  return (
    <div className="inline-flex items-center justify-end gap-3 text-sm">
      {items
        .toSorted((a, b) =>
          (a.person?.name ?? '').localeCompare(b.person?.name ?? '', 'cs'),
        )
        .map((item) => {
          const status = item.eventAttendance?.status;
          const Icon = status ? attendanceIcons[status] : null;
          return (
            <div
              key={item.id}
              className="inline-flex items-center justify-end gap-1 whitespace-nowrap"
            >
              <span className="text-neutral-11">
                {item.person?.firstName ?? item.person?.name}
              </span>
              {Icon ? <Icon className="size-4 shrink-0 text-neutral-12" /> : null}
            </div>
          );
        })}
    </div>
  );
}

function CompetitionTimelineCard({ items }: { items: CompetitionItem[] }) {
  const first = items[0];
  if (!first) return null;

  return (
    <div
      className={cardCls({
        className: 'w-full rounded-lg border-green-7 bg-green-2 p-3',
      })}
    >
      <CompetitionEventContent
        title={first.competitionEventName ?? ''}
        location={first.competitionEventLocation}
        entries={items.map(toCompetitionEntry)}
      />
    </div>
  );
}
