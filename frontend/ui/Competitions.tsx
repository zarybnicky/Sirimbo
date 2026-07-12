import {
  ActivityTimelineDocument,
  type ActivityTimelineItem_ActivityCompetitionBrief_Fragment,
  type ActivityTimelineItem_ActivityCompetitionResult_Fragment,
} from '@/graphql/ActivityTimeline';
import type { ActivityTimelineKind } from '@/graphql';
import { formatCstsCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { numericDateFormatter } from '@/ui/format';
import { Checkbox } from '@/ui/fields/checkbox';
import { cardCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { WeekPicker } from '@/ui/WeekPicker';
import { add, startOf } from 'date-arithmetic';
import { ExternalLink } from 'lucide-react';
import * as React from 'react';
import { useQuery } from 'urql';

type TimelineCompetitionItem =
  | ActivityTimelineItem_ActivityCompetitionBrief_Fragment
  | ActivityTimelineItem_ActivityCompetitionResult_Fragment;
export type CompetitionEntry = TimelineCompetitionItem;

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

function formatRank(row: Pick<ActivityTimelineItem_ActivityCompetitionResult_Fragment, 'ranking' | 'rankingTo'>) {
  if (!row.ranking) return '';
  return row.rankingTo && row.rankingTo !== row.ranking
    ? `${row.ranking}-${row.rankingTo}.`
    : `${row.ranking}.`;
}

function cstsResultSourceUrl(entry: ActivityTimelineItem_ActivityCompetitionResult_Fragment) {
  if (entry.federation !== 'csts') return null;
  const event = Number(entry.competitionEventExternalId);
  const competition = Number(entry.competitionExternalId);
  return event && competition
    ? `https://www.csts.cz/dancesport/vysledky_soutezi/event/${event}/competition/${competition}`
    : null;
}

function cstsUpcomingCalendarUrl(entries: readonly CompetitionEntry[]) {
  return entries.some(
    (entry) => entry.federation === 'csts' && entry.__typename === 'ActivityCompetitionBrief',
  )
    ? `https://www.csts.cz/dancesport/kalendar_akci`
    : null;
}

function groupByCompetitor(items: readonly CompetitionEntry[] | null | undefined) {
  const seen = new Set<string>();
  const groups = new Map<string, CompetitorGroup>();

  for (const item of items ?? []) {
    const dedupeKey = `${item.competitionId}:${item.competitorId}`;
    if (seen.has(dedupeKey)) continue;
    seen.add(dedupeKey);

    const groupKey = item.competitorId ?? '';
    const group = groups.get(groupKey) ?? {
      key: groupKey,
      competitorName: item.competitorName ?? '',
      entries: [],
    };

    group.entries.push(item);
    groups.set(groupKey, group);
  }

  return [...groups.values()].toSorted((a, b) => a.competitorName.localeCompare(b.competitorName));
}

function groupByDayEvent(rows: readonly CompetitionEntry[] | null | undefined) {
  const days = new Map<string, Map<string, CompetitionEntry[]>>();

  for (const row of rows ?? []) {
    const dayKey = row.competitionDate ?? '';
    const eventKey = `${row.competitionEventId}:${row.competitionEventName}:${row.competitionEventLocation}`;
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
        .map(([eventKey, entries]) => ({
          key: eventKey,
          eventName: entries[0]?.competitionEventName ?? '',
          eventLocation: entries[0]?.competitionEventLocation ?? '',
          sourceUrl: cstsUpcomingCalendarUrl(entries),
          competitorGroups: groupByCompetitor(entries),
        }))
        .toSorted((a, b) => a.eventName.localeCompare(b.eventName) || a.eventLocation.localeCompare(b.eventLocation)),
    }));
}

function CompetitionPanelFrame({ entries }: { entries: readonly CompetitionEntry[] }) {
  return (
    <>
      {groupByDayEvent(entries).map((day) => (
        <React.Fragment key={day.key}>
          {day.eventGroups.map((event) => (
            <section key={event.key} className="flex flex-col gap-1 justify-start">
              <div className="min-w-0 pt-2">
                <h6 className="text-xs text-neutral-11">
                  {event.eventLocation ?? ''}
                  {', '}
                  {numericDateFormatter.format(
                    new Date(`${day.competitionDate}T00:00:00`),
                  )}
                </h6>
                <div className="text-sm font-semibold leading-tight text-neutral-12">
                  <CompetitionSourceName href={event.sourceUrl}>
                    {event.eventName ?? ''}
                  </CompetitionSourceName>
                </div>
              </div>
              {event.competitorGroups.map((group) => (
                <article
                  key={group.key}
                  className={cardCls({
                    className: 'rounded-lg border-neutral-6 p-2 pb-1',
                  })}
                >
                  <CompetitionCompetitorGroup group={group} />
                </article>
              ))}
            </section>
          ))}
        </React.Fragment>
      ))}
    </>
  );
}

function CompetitionCompetitorGroup({ group }: { group: CompetitorGroup }) {
  const briefEntries = group.entries
    .filter((entry) => entry.__typename === 'ActivityCompetitionBrief')
    .toSorted((a, b) => {
      return (
        (a.checkInEnd ?? '').localeCompare(b.checkInEnd ?? '') ||
        (a.category?.name ?? '').localeCompare(b.category?.name ?? '')
      );
    });
  const reportEntries = group.entries
    .filter((entry) => entry.__typename === 'ActivityCompetitionResult')
    .toSorted(
      (a, b) =>
        (a.ranking ?? Number.MAX_SAFE_INTEGER) - (b.ranking ?? Number.MAX_SAFE_INTEGER) ||
        (a.category?.name ?? '').localeCompare(b.category?.name ?? ''),
    );

  return (
    <>
      <div className="flex items-start justify-between gap-3 min-w-0 pb-1">
        <h5 className="break-words text-sm font-medium leading-tight text-neutral-12">
          {group.competitorName}
        </h5>
      </div>
      {briefEntries.map((entry) => (
        <div
          key={entry.id}
          className="competition-entry-row grid grid-cols-[3rem_minmax(0,1fr)] items-start gap-2 py-1.5 text-xs"
        >
          <div className="font-semibold tabular-nums text-neutral-11">
            {entry.checkInEnd ? entry.checkInEnd.split(':').slice(0, 2).join(':') : ''}
          </div>
          <div className="flex min-w-0 items-center gap-1.5">
            <span className="h-3 w-1 rounded-sm" aria-hidden />
            <CompetitionCategoryLine entry={entry} />
          </div>
        </div>
      ))}
      {reportEntries.map((entry) => (
        <div
          key={entry.id}
          className="competition-entry-row grid grid-cols-[3rem_minmax(0,1fr)_min-content] items-center gap-2 py-1.5 text-xs"
        >
          <div className="font-semibold tabular-nums leading-none text-neutral-11">
            <span className={entry.ranking && entry.ranking <= 3 ? 'text-accent-11' : ''}>
              {formatRank(entry)}
            </span>
            {` z ${entry.participants ?? ''}`}
          </div>
          <CompetitionCategoryLine entry={entry} />
          <div className="justify-self-end whitespace-nowrap font-semibold tabular-nums text-green-11">
            {Number(entry.pointGain ?? 0) > 0 || entry.isFinal
              ? `+${Number(entry.pointGain ?? 0).toString()}b ${entry.isFinal ? 'F' : ''}`
              : ''}
          </div>
        </div>
      ))}
    </>
  );
}

function CompetitionCategoryLine({ entry }: { entry: CompetitionEntry }) {
  const sourceUrl =
    entry.__typename === 'ActivityCompetitionResult' ? cstsResultSourceUrl(entry) : null;
  const name = formatCstsCategoryName(entry.category, entry.competitionType);

  return (
    <div className="flex min-w-0 items-center gap-1.5 font-semibold text-neutral-12">
      <CompetitionSourceName
        href={sourceUrl}
        className="text-accent-12 hover:text-accent-11"
      >
        {name}
      </CompetitionSourceName>
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
        <CompetitionSourceName
          href={cstsUpcomingCalendarUrl(entries)}
          className="text-green-11 hover:text-green-12"
        >
          {title}
        </CompetitionSourceName>
      </div>
      {location && (
        <div className="truncate text-xs leading-tight text-green-11">{location}</div>
      )}
      <div>
        {groupByCompetitor(entries).map((group) => (
          <div key={group.key} className="border-t border-green-6 pt-2 first:border-t-0">
            <CompetitionCompetitorGroup group={group} />
          </div>
        ))}
      </div>
    </>
  );
}

function CompetitionSourceName({
  href,
  children,
  className,
}: {
  href?: string | null;
  children: React.ReactNode;
  className?: string;
}) {
  if (!href) return <span className="min-w-0">{children}</span>;

  return (
    <a
      href={href}
      target="_blank"
      rel="noreferrer"
      className={cn(
        'inline-flex min-w-0 items-center gap-1 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
    >
      <span className="min-w-0">{children}</span>
      <ExternalLink className="size-3 shrink-0" aria-hidden="true" />
    </a>
  );
}

export function CompetitionWeekPanel({
  personIds,
  cohortId,
  allowOnlyMine = false,
}: {
  allowOnlyMine?: boolean;
} & CompetitionScope) {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));
  const [onlyMine, setOnlyMine] = React.useState(allowOnlyMine);
  const myPersonIds = React.useMemo(() => new Set(auth.personIds), [auth.personIds]);
  const { mode, todayKey, variables } = React.useMemo(() => {
    const weekUntil = add(startDate, 1, 'week');
    const today = startOf(new Date(), 'day');
    const mode = weekUntil <= today ? 'past' : startDate > today ? 'future' : 'current';

    return {
      mode,
      todayKey: toLocalDateInput(today),
      variables: {
        since: startDate.toISOString(),
        until: weekUntil.toISOString(),
        personIds,
        cohortId,
        kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT'] satisfies ActivityTimelineKind[],
      },
    };
  }, [cohortId, personIds, startDate]);

  const [{ data, fetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables,
    requestPolicy: 'cache-and-network',
  });

  const { briefs, reports, locations } = React.useMemo(() => {
    const entries = (data?.activityTimelineList ?? [])
      .filter(
        (
          item,
        ): item is
          | ActivityTimelineItem_ActivityCompetitionBrief_Fragment
          | ActivityTimelineItem_ActivityCompetitionResult_Fragment =>
          item.__typename === 'ActivityCompetitionBrief' ||
          item.__typename === 'ActivityCompetitionResult',
      );

    const isRelevant = (entry: CompetitionEntry) =>
      (entry.__typename === 'ActivityCompetitionBrief'
        ? mode !== 'past' &&
          (mode !== 'current' || (entry.competitionDate ?? '') >= todayKey)
        : mode !== 'future');
    const visible = (entry: CompetitionEntry) =>
      isRelevant(entry) && (!onlyMine || myPersonIds.has(entry.personId ?? '-'));

    const locations = new Set<string>();
    for (const entry of entries) {
      if (isRelevant(entry) && entry.competitionEventLocation) {
        locations.add(entry.competitionEventLocation);
      }
    }

    return {
      briefs: entries.filter(
        (entry): entry is ActivityTimelineItem_ActivityCompetitionBrief_Fragment =>
          entry.__typename === 'ActivityCompetitionBrief' && visible(entry),
      ),
      reports: entries.filter(
        (entry): entry is ActivityTimelineItem_ActivityCompetitionResult_Fragment =>
          entry.__typename === 'ActivityCompetitionResult' && visible(entry),
      ),
      locations: [...locations],
    };
  }, [data?.activityTimelineList, mode, myPersonIds, onlyMine, todayKey]);

  const hasVisibleEntries = briefs.length > 0 || reports.length > 0;

  return (
    <section className="flex flex-col relative">
      <WeekPicker title="Soutěže" startDate={startDate} onChange={setStartDate} />

      {allowOnlyMine && (
        <div className="absolute top-7 right-0">
          <Checkbox
            name="competitions-only-mine"
            label="Pouze moje"
            checked={onlyMine}
            onChange={(event) => setOnlyMine(event.currentTarget.checked)}
            className="mt-1 shrink-0"
          />
        </div>
      )}

      <div className="text-sm text-neutral-9">
        {fetching ? 'Načítám...' : ''}
        {!fetching && !hasVisibleEntries ? 'Žádné soutěže tento týden. ' : ''}
        <br />
        {!fetching && !hasVisibleEntries && onlyMine && locations.length > 0 && (
          <a
            className="underline text-accent-12"
            href="#"
            onClick={(e) => {
              e.preventDefault();
              setOnlyMine(false);
            }}
          >
            Další soutěže s klubovou účastí: {locations.join(', ')}.
          </a>
        )}
      </div>

      {mode === 'current' ? (
        <div className="flex flex-col gap-4">
          <CompetitionPanelFrame entries={reports} />
          <CompetitionPanelFrame entries={briefs} />
        </div>
      ) : (
        <CompetitionPanelFrame entries={mode === 'past' ? reports : briefs} />
      )}
    </section>
  );
}
