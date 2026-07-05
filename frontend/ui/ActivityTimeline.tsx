import {
  ActivityTimelineDocument,
  type ActivityTimelineItemFragment,
  type ActivityTimelineItem_ActivityEventAttendance_Fragment,
  type ActivityTimelineItem_ActivityCompetitionBrief_Fragment,
  type ActivityTimelineItem_ActivityCompetitionResult_Fragment,
  type ActivityTimelineItem_ActivityJudging_Fragment,
} from '@/graphql/ActivityTimeline';
import type { ActivityTimelineKind, AttendanceType, EventType } from '@/graphql';
import { cn } from '@/lib/cn';
import { CompetitionEventContent } from '@/ui/Competitions';
import { formatCstsCategoryName } from '@/ui/csts';
import { EventButton } from '@/ui/EventButton';
import { attendanceIcons } from '@/ui/InstanceAttendanceView';
import { formatWeekDay } from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { add, subtract } from 'date-arithmetic';
import { Cake } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { CstsResultsLink } from './csts-links';

type TimelineMode = 'future' | 'past';
type TimelineFilter = EventType | 'COMPETITION' | 'JUDGING' | 'BIRTHDAY';
type TimelineItem = NonNullable<ActivityTimelineItemFragment>;
type EventAttendanceItem = ActivityTimelineItem_ActivityEventAttendance_Fragment;
type CompetitionItem =
  | ActivityTimelineItem_ActivityCompetitionBrief_Fragment
  | ActivityTimelineItem_ActivityCompetitionResult_Fragment;
type JudgingItem = ActivityTimelineItem_ActivityJudging_Fragment;

const RANGE_WEEKS = 8;
const BASE_FILTERS = [
  ['GROUP', 'Společná'],
  ['LESSON', 'Lekce'],
  ['CAMP', 'Soustředění'],
  ['RESERVATION', 'Nabídka'],
  ['HOLIDAY', 'Prázdniny'],
  ['COMPETITION', 'Soutěže'],
  ['BIRTHDAY', 'Narozeniny'],
] as const satisfies ReadonlyArray<readonly [TimelineFilter, string]>;
const JUDGING_FILTER = ['JUDGING', 'Porota'] as const satisfies readonly [
  TimelineFilter,
  string,
];

function rangeFor(mode: TimelineMode, pages: number) {
  const now = new Date();
  const weeks = RANGE_WEEKS * pages;
  return mode === 'future'
    ? { since: now, until: add(now, weeks, 'week') }
    : { since: subtract(now, weeks, 'week'), until: now };
}

export function ActivityTimeline({
  personIds,
  cohortId,
  includeJudging = false,
}: {
  personIds?: string[];
  cohortId?: string;
  includeJudging?: boolean;
}) {
  const [mode, setMode] = React.useState<TimelineMode>('future');
  const [pages, setPages] = React.useState(1);
  const availableFilters = React.useMemo(
    () => (includeJudging ? [...BASE_FILTERS, JUDGING_FILTER] : BASE_FILTERS),
    [includeJudging],
  );
  const [filters, setFilters] = React.useState<TimelineFilter[]>(() =>
    availableFilters
      .map(([value]) => value)
      .filter((x) => (cohortId ? x !== 'LESSON' : true)),
  );
  const scopeKey = `${cohortId ?? ''}:${personIds?.join(',') ?? ''}`;
  const range = React.useMemo(() => rangeFor(mode, pages), [mode, pages]);
  const eventTypes = filters.filter(
    (value): value is EventType =>
      value !== 'COMPETITION' && value !== 'JUDGING' && value !== 'BIRTHDAY',
  );
  const kinds: ActivityTimelineKind[] = eventTypes.length > 0 ? ['EVENT_ATTENDANCE'] : [];
  if (filters.includes('COMPETITION')) {
    kinds.push('COMPETITION_BRIEF', 'COMPETITION_RESULT');
  }
  if (filters.includes('JUDGING')) {
    kinds.push('JUDGING');
  }
  if (filters.includes('BIRTHDAY')) {
    kinds.push('BIRTHDAY');
  }

  React.useEffect(() => setPages(1), [scopeKey]);
  React.useEffect(() => {
    setFilters((value) =>
      value.filter(
        (filter) =>
          availableFilters.some(([availableFilter]) => availableFilter === filter) &&
          (!cohortId || filter !== 'LESSON'),
      ),
    );
  }, [availableFilters, cohortId]);

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
          {availableFilters.map(([filter, label]) => (
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
            isCohort={!!cohortId}
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
  const eventGroups = new Map<string, [EventAttendanceItem, ...EventAttendanceItem[]]>();
  const competitionGroups = new Map<string, [CompetitionItem, ...CompetitionItem[]]>();
  const judgingGroups = new Map<string, [JudgingItem, ...JudgingItem[]]>();

  const birthdayItems = items.filter(x => x.__typename === 'ActivityBirthday');

  for (const item of items) {
    if (item.__typename === 'ActivityEventAttendance' && item.eventInstance) {
      const key = item.eventInstance.id;
      eventGroups.set(key, [...(eventGroups.get(key) ?? []), item]);
    } else if (
      item.__typename === 'ActivityCompetitionBrief' ||
      item.__typename === 'ActivityCompetitionResult'
    ) {
      const key = item.competitionEventId!;
      competitionGroups.set(key, [...(competitionGroups.get(key) ?? []), item]);
    } else if (item.__typename === 'ActivityJudging') {
      const key = item.competitionEventId!;
      judgingGroups.set(key, [...(judgingGroups.get(key) ?? []), item]);
    }
  }

  const eventRows = [...eventGroups.values()].toSorted((a, b) =>
    (a[0]?.eventInstance?.since ?? '').localeCompare(b[0]?.eventInstance?.since ?? ''),
  );

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
        {birthdayItems.length > 0 && (
          <div className={cardCls({ className: 'bg-neutral-2' })}>
            <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-neutral-12">
              <Cake className="size-4 text-accent-11" />
              Narozeniny
            </div>
            <div className="flex flex-wrap gap-2">
              {birthdayItems.map((item) => (
                <Link
                  key={item.id}
                  href={`/clenove/${item.personId!}`}
                  className="rounded border border-neutral-6 bg-neutral-1 px-2 py-1 text-sm text-neutral-12 underline-offset-2 hover:bg-neutral-3 hover:underline"
                >
                  {item.person?.name}
                </Link>
              ))}
            </div>
          </div>
        )}

        {competitionGroups.size > 0 ? (
          <div className="flex flex-wrap justify-start gap-2 opacity-90">
            {competitionGroups.entries().map(([key, items]) => (
              <div
                key={`competition:${key}`}
                className={cardCls({
                  className: 'w-full rounded-lg border-green-7 bg-green-2 p-3',
                })}
              >
                <CompetitionEventContent
                  title={items[0].competitionEventName ?? ''}
                  location={items[0].competitionEventLocation}
                  entries={items}
                />
              </div>
            ))}
          </div>
        ) : null}

        {judgingGroups.size > 0 ? (
          <div className="flex flex-wrap justify-start gap-2 opacity-90">
            {judgingGroups.entries().map(([key, items]) => (
              <div
                key={`judging:${key}`}
                className={cardCls({
                  className: 'w-full rounded-lg border-accent-6 bg-accent-2 p-3',
                })}
              >
                <div className="mb-1 text-sm font-semibold leading-tight text-accent-11">
                  {items[0].competitionEventName}
                </div>
                <div className="text-xs leading-tight text-accent-11">
                  {items[0].competitionEventLocation}
                </div>
                <div className="mt-2 flex flex-col items-start gap-1">
                  {items
                    .filter(x => x.competitionId)
                    .toSorted((a, b) => (a.competitionId ?? '').localeCompare(b.competitionId ?? ''))
                    .map((item) => (
                      <CstsResultsLink
                        key={item.id}
                        className="text-xs text-neutral-12"
                        eventId={item.competitionEventExternalId}
                        competitionId={item.competitionExternalId}
                      >
                        {formatCstsCategoryName(item.category, item.competitionType)}
                      </CstsResultsLink>
                    ))}
                </div>
              </div>
            ))}
          </div>
        ) : null}

        {eventRows.length > 0 ? (
          <div className={cardCls({ className: 'rounded-lg border-neutral-6 border p-0' })}>
            {eventRows.map((items) => (
              <EventButton
                key={items[0].id}
                event={items[0].eventInstance?.event!}
                instance={items[0].eventInstance!}
                viewer="auto"
                suffix={
                  ((mode === 'past' && items[0].eventInstance?.type !== 'LESSON') || isCohort) ? (
                    <AttendanceSummary
                      compact={isCohort && items[0].eventInstance?.type !== 'LESSON'}
                      items={items}
                    />
                  ) : undefined
                }
              />
            ))}
          </div>
        ) : null}
      </div>
    </section>
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
        .toSorted((a, b) => (a.person?.name ?? '').localeCompare(b.person?.name ?? ''))
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
