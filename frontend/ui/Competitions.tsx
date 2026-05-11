import {
  CompetitionBriefDocument,
  CompetitionReportDocument,
  type CompetitionBriefQuery,
  type CompetitionReportQuery,
} from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { formatCstsCategoryName } from '@/ui/csts';
import { numericDateFormatter } from '@/ui/format';
import { Checkbox } from '@/ui/fields/checkbox';
import { cardCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { WeekPicker } from '@/ui/WeekPicker';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';

type BriefRow = NonNullable<CompetitionBriefQuery['competitionBriefList']>[number];
type ReportRow = NonNullable<CompetitionReportQuery['competitionReportList']>[number];

export type CompetitionBriefEntry = BriefRow & { kind: 'brief' };
export type CompetitionReportEntry = ReportRow & { kind: 'report' };
export type CompetitionEntry = CompetitionBriefEntry | CompetitionReportEntry;

type CompetitorGroup = {
  key: string;
  competitorName: string;
  entries: CompetitionEntry[];
};

type CompetitionScope = {
  personIds?: string[];
  cohortId?: string;
};

function toLocalDateInput(date: Date) {
  const year = date.getFullYear();
  const month = `${date.getMonth() + 1}`.padStart(2, '0');
  const day = `${date.getDate()}`.padStart(2, '0');
  return `${year}-${month}-${day}`;
}

function formatDate(date: string | null | undefined) {
  return date ? numericDateFormatter.format(new Date(`${date}T00:00:00`)) : '';
}

function formatTime(time: string | null | undefined) {
  return time ? time.split(':').slice(0, 2).join(':') : '';
}

function formatRank(row: Pick<CompetitionReportEntry, 'ranking' | 'rankingTo'>) {
  if (!row.ranking) return '';
  return row.rankingTo && row.rankingTo !== row.ranking
    ? `${row.ranking}-${row.rankingTo}.`
    : `${row.ranking}.`;
}

export function competitionEntryKey(entry: CompetitionEntry) {
  const category = entry.category;
  return [
    entry.kind,
    entry.eventId ?? '',
    entry.competitionDate ?? '',
    entry.competitionId ?? '',
    entry.competitorId ?? entry.personId ?? competitionEntryName(entry),
    category?.series ?? '',
    category?.discipline ?? '',
    category?.ageGroup ?? '',
    category?.class ?? '',
    category?.name ?? '',
  ].join(':');
}

export function competitionEntryName(entry: CompetitionEntry) {
  return entry.competitorName ?? entry.personName ?? 'Soutěžící';
}

export function formatCompetitionEntryLine(entry: CompetitionEntry) {
  const meta = entry.kind === 'brief'
    ? formatTime(entry.checkInEnd)
    : formatRank(entry);

  return [
    competitionEntryName(entry),
    formatCstsCategoryName(entry.category),
    meta,
  ]
    .filter(Boolean)
    .join(' · ');
}

const disciplineBarClass: Record<string, string> = {
  standard: 'bg-accent-9',
  latin: 'bg-green-9',
  '10-dance': 'bg-neutral-10',
};

function disciplineClass(row: CompetitionEntry) {
  const discipline = row.category?.discipline?.toLowerCase() ?? '';
  return disciplineBarClass[discipline] ?? 'bg-neutral-8';
}

function groupByCompetitor(rows: readonly CompetitionEntry[] | null | undefined) {
  const seen = new Set<string>();
  const groups = new Map<string, CompetitorGroup>();

  for (const row of rows ?? []) {
    const dedupeKey = competitionEntryKey(row);
    if (seen.has(dedupeKey)) continue;
    seen.add(dedupeKey);

    const groupKey = row.competitorId ?? row.personId ?? competitionEntryName(row);
    const group = groups.get(groupKey) ?? {
      key: groupKey,
      competitorName: competitionEntryName(row),
      entries: [],
    };

    group.entries.push(row);
    groups.set(groupKey, group);
  }

  return [...groups.values()].toSorted((a, b) =>
    a.competitorName.localeCompare(b.competitorName, 'cs'),
  );
}

function groupByDayEvent(rows: readonly CompetitionEntry[] | null | undefined) {
  const days = new Map<string, Map<string, CompetitionEntry[]>>();

  for (const row of rows ?? []) {
    const dayKey = row.competitionDate ?? '';
    const eventKey = `${row.eventId ?? ''}:${row.eventName ?? ''}:${row.eventLocation ?? ''}`;
    const day = days.get(dayKey) ?? new Map<string, CompetitionEntry[]>();
    const entries = day.get(eventKey) ?? [];
    entries.push(row);
    day.set(eventKey, entries);
    days.set(dayKey, day);
  }

  return [...days.entries()]
    .toSorted(([a], [b]) => String(a ?? '').localeCompare(String(b ?? '')))
    .map(([dayKey, events]) => ({
      key: dayKey,
      competitionDate: dayKey || null,
      eventGroups: [...events.entries()]
        .map(([eventKey, entries]) => {
          const first = entries[0];
          return {
            key: eventKey,
            eventName: first?.eventName ?? '',
            eventLocation: first?.eventLocation ?? '',
            competitorGroups: groupByCompetitor(entries),
          };
        })
        .toSorted((a, b) => {
          const byName = a.eventName.localeCompare(b.eventName, 'cs');
          return byName || a.eventLocation.localeCompare(b.eventLocation, 'cs');
        }),
    }));
}

function CompetitionPanelFrame({
  title,
  fetching,
  entries,
  emptyText,
}: {
  title: string;
  fetching: boolean;
  entries: readonly CompetitionEntry[];
  emptyText: string;
}) {
  const empty = entries.length === 0;

  return (
    <section className="flex flex-col">
      {title ? (
        <h5 className="mb-2 text-sm font-semibold uppercase tracking-wide text-neutral-11">
          {title}
        </h5>
      ) : null}

      {fetching ? <p className="text-sm text-neutral-9">Načítám...</p> : null}
      {!fetching && empty ? (
        <p className="text-xs text-neutral-8">{emptyText}</p>
      ) : null}

      {!empty ? (
        <CompetitionPanelGroups entries={entries} />
      ) : null}
    </section>
  );
}

function BriefRows({ entries }: { entries: CompetitionBriefEntry[] }) {
  const sorted = entries.toSorted((a, b) => {
    const byTime = String(a.checkInEnd ?? '').localeCompare(
      String(b.checkInEnd ?? ''),
    );
    return (
      byTime ||
      formatCstsCategoryName(a.category).localeCompare(
        formatCstsCategoryName(b.category),
        'cs',
      )
    );
  });

  return (
    <div className="divide-y divide-neutral-4">
      {sorted.map((entry) => (
        <div
          key={competitionEntryKey(entry)}
          className="grid grid-cols-[2.75rem_minmax(0,1fr)] items-start gap-2 py-1.5 text-xs"
        >
          <div className="font-mono font-semibold tabular-nums text-neutral-11">
            {formatTime(entry.checkInEnd)}
          </div>
          <div className="min-w-0">
            <div className="flex min-w-0 items-center gap-1.5">
              <span
                className={cn('h-3 w-1 rounded-sm', disciplineClass(entry))}
                aria-hidden
              />
              <span className="truncate font-semibold text-neutral-12">
                {formatCstsCategoryName(entry.category)}
              </span>
            </div>
            <div className="mt-0.5 truncate text-[0.7rem] text-neutral-10">
              {entry.category?.series ?? ''}
              {entry.dances?.filter(Boolean).length
                ? ` / ${entry.dances.filter(Boolean).join(', ')}`
                : ''}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

function ReportRows({ entries }: { entries: CompetitionReportEntry[] }) {
  const sorted = entries.toSorted((a, b) => {
    const byRank = (a.ranking ?? Number.MAX_SAFE_INTEGER) - (b.ranking ?? Number.MAX_SAFE_INTEGER);
    return (
      byRank ||
      formatCstsCategoryName(a.category).localeCompare(
        formatCstsCategoryName(b.category),
        'cs',
      )
    );
  });

  return (
    <div className="divide-y divide-neutral-4">
      {sorted.map((entry) => {
        const points = Number(entry.pointGain ?? 0);
        return (
          <div
            key={competitionEntryKey(entry)}
            className="grid grid-cols-[3.5rem_minmax(0,1fr)_auto] items-center gap-2 py-1.5 text-xs"
          >
            <div
              className={cn(
                'font-mono text-sm font-bold tabular-nums leading-none',
                entry.ranking && entry.ranking <= 3 ? 'text-accent-11' : 'text-neutral-11',
              )}
            >
              {formatRank(entry)}
            </div>
            <div className="min-w-0">
              <div className="flex min-w-0 items-center gap-1.5">
                <span
                  className={cn('h-3 w-1 rounded-sm', disciplineClass(entry))}
                  aria-hidden
                />
                <span className="truncate font-semibold text-neutral-12">
                  {formatCstsCategoryName(entry.category)}
                </span>
              </div>
              <div className="mt-0.5 truncate text-[0.7rem] text-neutral-10">
                {entry.category?.series ?? ''}
              </div>
            </div>
            <div className="flex min-w-20 items-baseline justify-end gap-1 font-mono tabular-nums">
              <span className="text-neutral-9">/{entry.participants ?? '-'}</span>
              {entry.isFinal ? (
                <span className="rounded-sm bg-accent-3 px-1 text-[0.65rem] font-bold text-accent-11">
                  F
                </span>
              ) : null}
              <span
                className={cn(
                  'font-semibold',
                  points > 0 ? 'text-green-11' : 'text-neutral-8',
                )}
              >
                {points > 0 ? `+${points.toFixed(1)}` : ''}
              </span>
            </div>
          </div>
        );
      })}
    </div>
  );
}

function CompetitionCompetitorGroup({
  group,
  variant = 'card',
}: {
  group: CompetitorGroup;
  variant?: 'card' | 'inline';
}) {
  const briefEntries = group.entries.filter((entry) => entry.kind === 'brief');
  const reportEntries = group.entries.filter((entry) => entry.kind === 'report');
  const body = (
    <>
      <div className="mb-2 flex items-start justify-between gap-3">
        <div className="min-w-0">
          <h5 className="break-words text-sm font-bold leading-tight text-neutral-12">
            {group.competitorName}
          </h5>
        </div>
      </div>
      {briefEntries.length > 0 ? <BriefRows entries={briefEntries} /> : null}
      {reportEntries.length > 0 ? <ReportRows entries={reportEntries} /> : null}
    </>
  );

  return variant === 'card' ? (
    <article className={cardCls({ className: 'rounded-lg border-neutral-6 p-3' })}>
      {body}
    </article>
  ) : (
    <div className="border-t border-green-6 py-2 first:border-t-0 first:pt-0 last:pb-0">
      {body}
    </div>
  );
}

export function CompetitionEventContent({
  title,
  location,
  entries,
}: {
  title: string;
  location?: string | null;
  entries: readonly CompetitionEntry[];
}) {
  return (
    <>
      <div className="mb-1 text-sm font-semibold leading-tight text-green-11">
        {title}
      </div>
      {location ? (
        <div className="mb-2 truncate text-xs leading-tight text-green-11">
          {location}
        </div>
      ) : null}
      <div className="space-y-1">
        {groupByCompetitor(entries).map((group) => (
          <CompetitionCompetitorGroup
            key={group.key}
            group={group}
            variant="inline"
          />
        ))}
      </div>
    </>
  );
}

function CompetitionPanelGroups({ entries }: { entries: readonly CompetitionEntry[] }) {
  return (
    <div className="space-y-5">
      {groupByDayEvent(entries).map((day) => (
        <section key={day.key} className="space-y-3">
          <h5 className="text-xs font-semibold uppercase tracking-wide text-neutral-10">
            {formatDate(day.competitionDate)}
          </h5>
          {day.eventGroups.map((event) => (
            <section key={event.key} className="space-y-2">
              <div className="min-w-0">
                <h6 className="text-sm font-semibold leading-tight text-neutral-12">
                  {event.eventName || 'Soutěž'}
                </h6>
                {event.eventLocation ? (
                  <div className="truncate text-xs text-neutral-10">
                    {event.eventLocation}
                  </div>
                ) : null}
              </div>
              <div className="space-y-2">
                {event.competitorGroups.map((group) => (
                  <CompetitionCompetitorGroup key={group.key} group={group} />
                ))}
              </div>
            </section>
          ))}
        </section>
      ))}
    </div>
  );
}

export function CompetitionWeekPanel({
  title = 'Soutěže a výsledky',
  personIds,
  cohortId,
  allowOnlyMine = false,
}: {
  title?: string;
  allowOnlyMine?: boolean;
} & CompetitionScope) {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));
  const [onlyMine, setOnlyMine] = React.useState(allowOnlyMine);
  const { mode, ranges } = React.useMemo(() => {
    const weekUntil = add(startDate, 1, 'week');
    const today = startOf(new Date(), 'day');
    const tomorrow = add(today, 1, 'day');
    const mode =
      weekUntil <= today ? 'past' : startDate > today ? 'future' : 'current';

    return {
      mode,
      ranges: {
        brief:
          mode === 'past'
            ? null
            : {
                since: mode === 'current' ? today : startDate,
                until: weekUntil,
              },
        report:
          mode === 'future'
            ? null
            : {
                since: mode === 'current' ? add(startDate, -1, 'week') : startDate,
                until: mode === 'current' ? tomorrow : weekUntil,
              },
      },
    };
  }, [startDate]);
  const scope = allowOnlyMine && onlyMine ? auth.personIds : personIds;
  const briefVariables = React.useMemo(
    () =>
      ranges.brief
        ? {
            since: toLocalDateInput(ranges.brief.since),
            until: toLocalDateInput(ranges.brief.until),
            personIds: scope,
            cohortId,
          }
        : {},
    [cohortId, ranges.brief, scope],
  );
  const reportVariables = React.useMemo(
    () =>
      ranges.report
        ? {
            since: toLocalDateInput(ranges.report.since),
            until: toLocalDateInput(ranges.report.until),
            personIds: scope,
            cohortId,
          }
        : {},
    [cohortId, ranges.report, scope],
  );
  const [{ data: briefData, fetching: fetchingBrief }] = useQuery({
    query: CompetitionBriefDocument,
    variables: briefVariables,
    pause: !ranges.brief,
  });
  const [{ data: reportData, fetching: fetchingReport }] = useQuery({
    query: CompetitionReportDocument,
    variables: reportVariables,
    pause: !ranges.report,
  });
  const briefEntries = React.useMemo<CompetitionBriefEntry[]>(
    () =>
      ranges.brief
        ? (briefData?.competitionBriefList ?? []).map((entry) => ({
            ...entry,
            kind: 'brief',
          }))
        : [],
    [briefData?.competitionBriefList, ranges.brief],
  );
  const reportEntries = React.useMemo<CompetitionReportEntry[]>(
    () =>
      ranges.report
        ? (reportData?.competitionReportList ?? []).map((entry) => ({
            ...entry,
            kind: 'report',
          }))
        : [],
    [ranges.report, reportData?.competitionReportList],
  );

  return (
    <section className="flex flex-col">
      <div className="flex flex-wrap items-start justify-between gap-x-4 gap-y-1">
        <WeekPicker title={title} startDate={startDate} onChange={setStartDate} />
        {allowOnlyMine ? (
          <Checkbox
            name={`${title}-only-mine`}
            label="Pouze moje"
            checked={onlyMine}
            onChange={(event) => setOnlyMine(event.currentTarget.checked)}
            className="mt-1 shrink-0"
          />
        ) : null}
      </div>

      {mode === 'current' ? (
        <div className="mt-2 space-y-4">
          <CompetitionPanelFrame
            title="Výsledky"
            fetching={fetchingReport}
            entries={reportEntries}
            emptyText="Žádné nedávné výsledky."
          />
          <CompetitionPanelFrame
            title="Soutěže"
            fetching={fetchingBrief}
            entries={briefEntries}
            emptyText="Žádné nadcházející soutěže."
          />
        </div>
      ) : (
        <div className="mt-2">
          <CompetitionPanelFrame
            title=""
            fetching={mode === 'past' ? fetchingReport : fetchingBrief}
            entries={mode === 'past' ? reportEntries : briefEntries}
            emptyText={mode === 'past' ? 'Žádné výsledky.' : 'Žádné soutěže.'}
          />
        </div>
      )}
    </section>
  );
}
