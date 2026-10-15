import {
  CompetitionParticipationsDocument,
  ClubCompetitionLocationsDocument,
  type CompetitionParticipationsQuery,
  type CompetitionParticipationsQueryVariables,
} from '@/graphql/CompetitionParticipation';
import type {
  CompetitionType,
  FederationEventStatus,
  CompetitionParticipantCondition,
} from '@/graphql';
import type { CompetitionCategoryFragment } from '@/graphql/Federation';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { format } from 'date-fns';
import { formatCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { formatDateOnly, formatResultStatus } from '@/ui/format';
import { Checkbox } from '@/ui/fields/checkbox';
import { cardCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { WeekPicker } from '@/ui/WeekPicker';
import { add, startOf } from 'date-arithmetic';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';

export type CompetitionEntry = {
  id: string;
  federation: string | null;
  competitorId: string | null;
  competitorName: string | null;
  competitionId: string | null;
  competitionExternalId: string | null;
  competitionEventId: string | null;
  competitionEventExternalId: string | null;
  competitionEventName: string | null;
  competitionEventLocation: string | null;
  competitionDate: string | null;
  competitionType: CompetitionType | null;
  category: CompetitionCategoryFragment | null;
  participants: number | null;
  checkInEnd?: string | null;
  status?: FederationEventStatus;
  ranking?: number | null;
  rankingTo?: number | null;
  pointGain?: string | null;
  finalGain?: string | null;
  isFinal?: boolean | null;
  completionStatus?: string | null;
};

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

  return [...days.entries()].map(([dayKey, events]) => ({
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
                  {day.competitionDate && formatDateOnly(day.competitionDate)}
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
          formatCategoryName(a.federation, a.category, a.competitionType).localeCompare(
            formatCategoryName(b.federation, b.category, b.competitionType),
          ),
        )
        .map((entry) => {
          const result = entry.ranking !== undefined ? entry : null;
          const status = formatResultStatus(entry.completionStatus);
          const category =
            formatCategoryName(entry.federation, entry.category, entry.competitionType) ||
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
                  : entry.checkInEnd?.slice(0, 5)}
              </div>
              <CompetitionLink
                entry={entry}
                competitionId={entry.competitionExternalId}
                className={cn(
                  'min-w-0 font-semibold text-accent-11',
                  entry.status === 'CANCELLED' && 'line-through',
                )}
              >
                {category}
              </CompetitionLink>
              {result && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-green-11">
                  {Number(result.pointGain) > 0 && `+${Number(result.pointGain)}b`}
                  {/* TODO: Use finalGain once existing ČSTS results have been reimported. */}
                  {entry.federation === 'csts' && result.isFinal && ' F'}
                </div>
              )}
              {status && <div className="col-span-full text-neutral-11">{status}</div>}
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

function competitionEntries(
  competitions: NonNullable<CompetitionParticipationsQuery['competitions']>['nodes'],
): CompetitionEntry[] {
  return competitions.flatMap((competition) => {
    const common = {
      federation: competition.federation,
      competitionId: competition.id,
      competitionExternalId: competition.externalId,
      competitionEventId: competition.event?.id ?? null,
      competitionEventExternalId: competition.event?.externalId ?? null,
      competitionEventName: competition.event?.name ?? null,
      competitionEventLocation:
        competition.event?.location || competition.event?.city || null,
      competitionDate: competition.startDate,
      competitionType: competition.competitionType,
      category: competition.category,
      participants: competition.participantsTotal,
      checkInEnd: competition.checkInEnd,
      status: competition.status,
    };
    return [
      ...competition.results.map((result) => ({
        ...common,
        ...result,
        id: `${competition.id}:${result.competitorId}`,
        competitorName: result.competitor?.name ?? result.competitorId,
      })),
      ...competition.entries.map((entry) => ({
        ...common,
        ...entry,
        id: `${competition.id}:${entry.competitorId}`,
        competitorName: entry.competitor?.name ?? entry.competitorId,
      })),
    ];
  });
}

export function CompetitionHistory({
  personId,
  coupleId,
}: { personId: string; coupleId?: never } | { coupleId: string; personId?: never }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionParticipationsDocument,
    variables: (offset): CompetitionParticipationsQueryVariables => ({
      participant: personId != null ? { personIds: [personId] } : { coupleId },
      orderBy: ['START_DATE_DESC', 'ID_DESC'],
      offset,
    }),
    limit: 50,
    pageKey: 'competitionPage',
    getTotal: (data) => data?.competitions?.totalCount,
  });

  return (
    <section className="my-4">
      <h2 className="mb-2 text-lg font-bold">Soutěže a výsledky</h2>
      {error ? (
        <FormError error={error} />
      ) : fetching ? (
        <p role="status" className="text-sm text-neutral-11">
          Načítám...
        </p>
      ) : data?.competitions?.nodes.length ? (
        <CompetitionPanelFrame entries={competitionEntries(data.competitions.nodes)} />
      ) : (
        <p className="text-sm text-neutral-11">Žádné registrace ani výsledky soutěží.</p>
      )}
      <Pagination {...pagination} />
    </section>
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
  const date = {
    since: format(startDate, 'yyyy-MM-dd'),
    until: format(add(startDate, 1, 'week'), 'yyyy-MM-dd'),
  };
  const clubScope: CompetitionParticipantCondition = {
    currentTenant: personIds == null,
    personIds,
    cohortId,
  };
  const participant: CompetitionParticipantCondition = {
    ...clubScope,
    personIds: onlyMine
      ? (personIds?.filter((id) => auth.personIds.includes(id)) ?? auth.personIds)
      : personIds,
  };
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionParticipationsDocument,
    variables: (offset) => ({ participant, date, offset }),
    limit: 50,
    pageKey: 'competitionPage',
    getTotal: (data) => data?.competitions?.totalCount,
  });
  const [{ data: clubData, fetching: clubFetching }] = useQuery({
    query: ClubCompetitionLocationsDocument,
    variables: { participant: clubScope, date },
    pause: !onlyMine || fetching || !!error || data?.competitions?.totalCount !== 0,
  });
  const visible = competitionEntries(data?.competitions?.nodes ?? []);
  const locations = [
    ...new Set(
      (clubData?.competitions?.nodes ?? []).flatMap(
        (competition) => competition.event?.location || competition.event?.city || [],
      ),
    ),
  ];

  return (
    <section className="flex flex-col relative">
      <WeekPicker
        title="Soutěže"
        startDate={startDate}
        onChange={(date) => {
          setStartDate(date);
          pagination.onPageChange(1);
        }}
      />

      {allowOnlyMine && (
        <div className="absolute top-7 right-0">
          <Checkbox
            name="competitions-only-mine"
            label="Pouze moje"
            checked={onlyMine}
            onChange={(event) => {
              setOnlyMine(event.currentTarget.checked);
              pagination.onPageChange(1);
            }}
            className="mt-1 shrink-0"
          />
        </div>
      )}

      {error ? (
        <FormError error={error} />
      ) : fetching ? (
        <p className="text-sm text-neutral-9">Načítám...</p>
      ) : (
        visible.length === 0 && (
          <div className="text-sm text-neutral-9">
            <p>Žádné soutěže tento týden.</p>
            {onlyMine && !clubFetching && !!clubData?.competitions?.nodes.length && (
              <button
                type="button"
                className="underline text-accent-12"
                onClick={() => {
                  setOnlyMine(false);
                  pagination.onPageChange(1);
                }}
              >
                Další soutěže s klubovou účastí
                {locations.length > 0 ? `: ${locations.join(', ')}` : ''}
                {clubData?.competitions?.pageInfo.hasNextPage ? '…' : '.'}
              </button>
            )}
          </div>
        )
      )}

      {!error && !fetching && <CompetitionPanelFrame entries={visible} />}
      <Pagination {...pagination} />
    </section>
  );
});
