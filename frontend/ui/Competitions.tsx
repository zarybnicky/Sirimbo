import {
  ActivityTimelineDocument,
  type ActivityTimelineItem_ActivityCompetitionBrief_Fragment,
  type ActivityTimelineItem_ActivityCompetitionResult_Fragment,
} from '@/graphql/ActivityTimeline';
import { formatCstsCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { numericDateFormatter } from '@/ui/format';
import { Checkbox } from '@/ui/fields/checkbox';
import { cardCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { WeekPicker } from '@/ui/WeekPicker';
import { add, startOf } from 'date-arithmetic';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';

export type CompetitionEntry =
  | ActivityTimelineItem_ActivityCompetitionBrief_Fragment
  | ActivityTimelineItem_ActivityCompetitionResult_Fragment;

type CompetitorGroup = {
  key: string;
  competitorName: string;
  entries: CompetitionEntry[];
};

type CompetitionScope = {
  personIds?: string[];
  cohortId?: string;
};

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

  return [...groups.values()].toSorted((a, b) =>
    a.competitorName.localeCompare(b.competitorName),
  );
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
          entry: entries[0],
          competitorGroups: groupByCompetitor(entries),
        }))
        .toSorted(
          (a, b) =>
            a.eventName.localeCompare(b.eventName) ||
            a.eventLocation.localeCompare(b.eventLocation),
        ),
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
                  <CompetitionLink entry={event.entry}>
                    {event.eventName ?? ''}
                  </CompetitionLink>
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
  return (
    <div className="grid grid-cols-[minmax(3rem,max-content)_minmax(0,1fr)_max-content] gap-x-2 @max-[99px]/competition:grid-cols-1">
      <h5 className="col-span-full wrap-break-word pb-1 text-sm font-medium leading-tight text-neutral-12">
        {group.competitorName}
      </h5>
      {group.entries
        .toSorted((a, b) =>
          (a.category?.name ?? '').localeCompare(b.category?.name ?? ''),
        )
        .map((entry) => {
          const result = entry.__typename === 'ActivityCompetitionResult' ? entry : null;
          const category =
            (entry.federation === 'csts'
              ? formatCstsCategoryName(entry.category, entry.competitionType)
              : entry.category?.name) ||
            entry.competitionExternalId ||
            entry.competitionId;
          return (
            <div
              key={entry.id}
              className="col-span-full grid grid-cols-subgrid items-baseline py-1.5 text-xs"
            >
              <div className="whitespace-nowrap font-semibold tabular-nums text-neutral-11">
                {result
                  ? result.ranking != null && (
                      <>
                        <span className={result.ranking <= 3 ? 'text-accent-11' : ''}>
                          {result.ranking}
                          {result.rankingTo && result.rankingTo !== result.ranking
                            ? `–${result.rankingTo}`
                            : ''}
                          .
                        </span>
                        {result.participants != null && ` z ${result.participants}`}
                      </>
                    )
                  : entry.__typename === 'ActivityCompetitionBrief'
                    ? entry.checkInEnd?.slice(0, 5)
                    : null}
              </div>
              <CompetitionLink
                entry={entry}
                competitionId={entry.competitionExternalId}
                className="min-w-0 font-semibold text-accent-11"
              >
                {category}
              </CompetitionLink>
              {result && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-green-11">
                  {Number(result.pointGain) > 0 && `+${Number(result.pointGain)}b`}
                  {result.isFinal && ' F'}
                </div>
              )}
            </div>
          );
        })}
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
        <CompetitionLink entry={entries[0]} className="text-green-11 hover:text-green-12">
          {title}
        </CompetitionLink>
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

export function CompetitionLink({
  entry,
  competitionId,
  children,
  className,
}: {
  entry?: Pick<CompetitionEntry, 'federation' | 'competitionEventExternalId'>;
  competitionId?: string | null;
  children: React.ReactNode;
  className?: string;
}) {
  if (!entry?.federation || !entry.competitionEventExternalId) return <>{children}</>;

  return (
    <Link
      href={`/federated/${entry.federation}/${entry.competitionEventExternalId}${competitionId ? `/${competitionId}` : ''}`}
      prefetch={false}
      className={cn('underline hover:text-accent-11', className)}
    >
      {children}
    </Link>
  );
}

export const CompetitionWeekPanel = React.memo(function CompetitionWeekPanel({
  personIds,
  cohortId,
  allowOnlyMine = false,
}: {
  allowOnlyMine?: boolean;
} & CompetitionScope) {
  const auth = useAuth();
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));
  const [onlyMine, setOnlyMine] = React.useState(allowOnlyMine);
  const [{ data, fetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: startDate.toISOString(),
      until: add(startDate, 1, 'week').toISOString(),
      personIds,
      cohortId,
      kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT'],
    },
    requestPolicy: 'cache-and-network',
  });
  const entries = (data?.activityTimelineList ?? []).filter(
    (item): item is CompetitionEntry =>
      item.__typename === 'ActivityCompetitionBrief' ||
      item.__typename === 'ActivityCompetitionResult',
  );
  const visible = onlyMine
    ? entries.filter((entry) => auth.personIds.includes(entry.personId ?? ''))
    : entries;
  const locations = [
    ...new Set(entries.flatMap((entry) => entry.competitionEventLocation || [])),
  ];

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

      {fetching ? (
        <p className="text-sm text-neutral-9">Načítám...</p>
      ) : (
        visible.length === 0 && (
          <div className="text-sm text-neutral-9">
            <p>Žádné soutěže tento týden.</p>
            {onlyMine && locations.length > 0 && (
              <button
                type="button"
                className="underline text-accent-12"
                onClick={() => setOnlyMine(false)}
              >
                Další soutěže s klubovou účastí: {locations.join(', ')}.
              </button>
            )}
          </div>
        )
      )}

      <CompetitionPanelFrame entries={visible} />
    </section>
  );
});
