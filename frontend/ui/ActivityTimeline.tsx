import {
  ActivityTimelineDocument,
  type ActivityTimelineItem_ActivityEventAttendance_Fragment,
  type ActivityTimelineItem_ActivityJudging_Fragment,
  type ActivityTimelineItemFragment,
} from '@/graphql/ActivityTimeline';
import type { ActivityTimelineKind, AttendanceType } from '@/graphql';
import { compareText } from '@/lib/compare';
import {
  CompetitionEventContent,
  CompetitionLink,
  groupCompetitionEvents,
} from '@/ui/Competitions';
import { formatCategoryName } from '@/ui/csts';
import { EventButton } from '@/ui/EventButton';
import { attendanceIcons } from '@/ui/EventAttendance';
import { FacetFilter } from '@/ui/FacetFilter';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { capitalize, longDayFormatter } from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { add, subtract } from 'date-arithmetic';
import { Cake } from 'lucide-react';
import * as React from 'react';
import { useQuery } from 'urql';
import { LocalPersonLinks } from './federation-links';

type TimelineMode = 'future' | 'past';
type EventAttendanceItem = ActivityTimelineItem_ActivityEventAttendance_Fragment;
type JudgingItem = ActivityTimelineItem_ActivityJudging_Fragment;

const FILTER_OPTIONS = [
  { value: 'GROUP', label: 'Společná' },
  { value: 'LESSON', label: 'Lekce' },
  { value: 'CAMP', label: 'Soustředění' },
  { value: 'RESERVATION', label: 'Nabídka' },
  { value: 'HOLIDAY', label: 'Prázdniny' },
  { value: 'COMPETITION', label: 'Soutěže' },
  { value: 'BIRTHDAY', label: 'Narozeniny' },
  { value: 'JUDGING', label: 'Porota' },
] as const;
type TimelineFilter = (typeof FILTER_OPTIONS)[number]['value'];

export const ActivityTimeline = React.memo(function ActivityTimeline({
  personIds,
  cohortId,
}: {
  personIds?: string[];
  cohortId?: string;
}) {
  const [mode, setMode] = React.useState<TimelineMode>('past');
  const [pages, setPages] = React.useState(1);
  const defaultFilters = FILTER_OPTIONS.map(({ value }) => value)
    .filter((x) => !cohortId || (x !== 'JUDGING' && x !== 'LESSON'));
  const [filters, setFilters] = React.useState<TimelineFilter[]>(defaultFilters);
  const range = React.useMemo(() => {
    const now = new Date();
    return mode === 'future'
      ? { since: now, until: add(now, 8 * pages, 'week') }
      : { since: subtract(now, 8 * pages, 'week'), until: now };
  }, [mode, pages]);
  const eventTypes = filters.filter(
    (x) => x !== 'COMPETITION' && x !== 'JUDGING' && x !== 'BIRTHDAY',
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
  });

  const days = React.useMemo(() => {
    const direction = mode === 'future' ? 1 : -1;
    const grouped = new Map<string, ActivityTimelineItemFragment[]>();
    for (const item of (data?.activityTimelineList ?? [])
      .filter(Boolean)
      .toSorted((a, b) => {
        const byTime = (a.sortAt ?? '').localeCompare(b.sortAt ?? '') * direction;
        return byTime || a.id.localeCompare(b.id);
      })) {
      const key = item.activityDate ?? item.sortAt?.slice(0, 10) ?? '';
      const group = grouped.get(key);
      if (group) group.push(item);
      else grouped.set(key, [item]);
    }
    return [...grouped.entries()];
  }, [data?.activityTimelineList, mode]);

  return (
    <>
      <div className="flex flex-wrap items-center gap-3">
        <RadioButtonGroup
          value={mode}
          options={[
            { id: 'future', label: 'Budoucí' },
            { id: 'past', label: 'Minulé' },
          ]}
          onValueChange={(value) => {
            setMode(value as TimelineMode);
            setPages(1);
          }}
        />

        <FacetFilter
          label="Typy"
          value={filters}
          defaultValue={defaultFilters}
          options={FILTER_OPTIONS}
          onChange={(value) => {
            setPages(1);
            setFilters(value);
          }}
        />
      </div>

      <div className="text-sm text-neutral-9 my-3">
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
          onClick={() => setPages((x) => x + 1)}
        >
          {mode === 'future' ? 'Načíst další...' : 'Načíst starší...'}
        </button>
      </div>
    </>
  );
});

function TimelineDay({
  date,
  items,
  mode,
  isCohort,
}: {
  date: string;
  items: ActivityTimelineItemFragment[];
  mode: TimelineMode;
  isCohort: boolean;
}) {
  const eventGroups = new Map<string, [EventAttendanceItem, ...EventAttendanceItem[]]>();
  const competitionGroups = groupCompetitionEvents(
    items.filter(
      (item) =>
        item.__typename === 'ActivityCompetitionBrief' ||
        item.__typename === 'ActivityCompetitionResult',
    ),
  );
  const judgingGroups = new Map<string, [JudgingItem, ...JudgingItem[]]>();

  const birthdayItems = items.filter((x) => x.__typename === 'ActivityBirthday');

  for (const item of items) {
    if (item.__typename === 'ActivityEventAttendance' && item.event) {
      const key = item.event.id;
      eventGroups.set(key, [...(eventGroups.get(key) ?? []), item]);
    } else if (item.__typename === 'ActivityJudging') {
      const key = item.competitionEventId!;
      judgingGroups.set(key, [...(judgingGroups.get(key) ?? []), item]);
    }
  }

  const eventRows = [...eventGroups.values()].toSorted((a, b) =>
    (a[0]?.event?.since ?? '').localeCompare(b[0]?.event?.since ?? ''),
  );

  return (
    <section>
      <h4 className="my-2 text-xl tracking-tight">
        {capitalize(longDayFormatter.format(new Date(`${date}T00:00:00`)))}
      </h4>

      <div className="flex flex-col gap-2">
        {birthdayItems.length > 0 && (
          <div className={cardCls({ className: 'bg-neutral-2' })}>
            <div className="mb-2 flex items-center gap-2 text-sm font-semibold text-neutral-12">
              <Cake className="size-4 text-accent-11" />
              Narozeniny
            </div>
            <div className="text-sm">
              <LocalPersonLinks people={birthdayItems.flatMap((x) => x.person ?? [])} />
            </div>
          </div>
        )}

        {competitionGroups.length > 0 && (
          <div className="flex flex-wrap justify-start gap-2 opacity-90">
            {competitionGroups.map(({ key, entries }) => (
              <div
                key={`competition:${key}`}
                className={cardCls({
                  className: 'w-full rounded-lg border-green-7 bg-green-2 p-3',
                })}
              >
                <CompetitionEventContent entries={entries} />
              </div>
            ))}
          </div>
        )}

        {judgingGroups.size > 0 && (
          <div className="flex flex-wrap justify-start gap-2 opacity-90">
            {[...judgingGroups].map(([key, items]) => (
              <div
                key={`judging:${key}`}
                className={cardCls({
                  className: 'w-full rounded-lg border-accent-6 bg-accent-2 p-3',
                })}
              >
                <div className="mb-1 text-sm font-semibold leading-tight text-accent-11">
                  <CompetitionLink entry={items[0]}>
                    {items[0].competitionEventName}
                  </CompetitionLink>
                </div>
                <div className="text-xs leading-tight text-accent-11">
                  {items[0].competitionEventLocation}
                </div>
                <div className="mt-2 flex flex-col items-start gap-1">
                  {items
                    .filter((x) => x.competitionId)
                    .toSorted((a, b) =>
                      (a.competitionId ?? '').localeCompare(b.competitionId ?? ''),
                    )
                    .map((item) => (
                      <CompetitionLink
                        key={item.id}
                        className="text-xs text-neutral-12"
                        entry={item}
                        competitionId={item.competitionExternalId}
                      >
                        {formatCategoryName(
                          item.federation,
                          item.category,
                          item.competitionType,
                        ) ||
                          item.competitionExternalId ||
                          item.competitionId}
                      </CompetitionLink>
                    ))}
                </div>
              </div>
            ))}
          </div>
        )}

        {eventRows.length > 0 && (
          <div
            className={cardCls({ className: 'rounded-lg border-neutral-6 border p-0' })}
          >
            {eventRows.map((items) => (
              <EventButton
                key={items[0].id}
                event={items[0].event!}
                viewer="auto"
                suffix={
                  ((mode === 'past' && items[0].event?.type !== 'LESSON') ||
                    isCohort) && (
                    <AttendanceSummary
                      compact={isCohort && items[0].event?.type !== 'LESSON'}
                      items={items}
                    />
                  )
                }
              />
            ))}
          </div>
        )}
      </div>
    </section>
  );
}

function attendanceCounts(items: EventAttendanceItem[]) {
  const counts: Record<AttendanceType, number> = {
    ATTENDED: 0,
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
        <NotExcusedIcon className="size-4 text-danger-11" />
        <span>{counts.NOT_EXCUSED}</span>
        <span>/ {items.length}</span>
      </div>
    );
  }

  return (
    <div className="inline-flex items-center justify-end gap-3 text-sm">
      {items
        .toSorted((a, b) => compareText(a.person?.name ?? '', b.person?.name ?? ''))
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
