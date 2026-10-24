import dynamic from 'next/dynamic';
import type { MapView } from '@/ui/EventMap';
import { DotSeparated } from '@/ui/DotSeparated';
import {
  CompetitionParticipationsDocument,
  CompetitionSuggestionsDocument,
  CompetitionSuggestionEventsDocument,
  type ParticipationCompetitionFragment,
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
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { format } from 'date-fns';
import { formatCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { compareText } from '@/lib/compare';
import {
  formatCount,
  formatDateOnly,
  formatPointsAndFinals,
  formatResultStatus,
} from '@/ui/format';
import { Checkbox } from '@/ui/fields/checkbox';
import { cardCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { WeekPicker } from '@/ui/WeekPicker';
import { add, startOf } from 'date-arithmetic';
import { ArrowLeftRight } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';

const EventMap = dynamic(() => import('@/ui/EventMap'));

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
  completionStatus?: string | null;
  cancelled?: boolean;
};

type CompetitionScope = {
  personIds?: string[];
  cohortId?: string;
};

function entryLabel(
  entry: Pick<CompetitionEntry, 'federation' | 'category' | 'competitionType'> & {
    competitorName?: string | null;
  },
  competition: boolean,
) {
  return competition
    ? formatCategoryName(entry.federation, entry.category, entry.competitionType) || ''
    : (entry.competitorName ?? '');
}

function checkInTime(entries: readonly CompetitionEntry[]) {
  return entries.find((x) => x.checkInEnd)?.checkInEnd;
}

export function groupCompetitionEvents(entries: readonly CompetitionEntry[]) {
  return [...Map.groupBy(entries, (x) => x.competitionDate).values()].flatMap((day) =>
    [...Map.groupBy(day, (x) => x.competitionEventId)]
      .map(([eventId, entries]) => ({
        key: `${entries[0]!.competitionDate ?? ''}:${eventId ?? ''}`,
        entries,
      }))
      .toSorted(
        (a, b) =>
          compareText(
            a.entries[0]!.competitionEventName ?? '',
            b.entries[0]!.competitionEventName ?? '',
          ) ||
          compareText(
            a.entries[0]!.competitionEventLocation ?? '',
            b.entries[0]!.competitionEventLocation ?? '',
          ),
      ),
  );
}

function CompetitionPanelFrame({ entries }: { entries: readonly CompetitionEntry[] }) {
  return groupCompetitionEvents(entries).map(({ key, entries }) => (
    <section key={key} className="pt-2">
      <CompetitionEventContent entries={entries} cards />
    </section>
  ));
}

export function CompetitionEventContent({
  entries,
  cards = false,
}: {
  entries: readonly CompetitionEntry[];
  cards?: boolean;
}) {
  const [override, setOverride] = React.useState<boolean | null>(null);
  const event = entries[0];
  if (!event) return null;
  const unique = new Map<string, CompetitionEntry>();
  for (const entry of entries) {
    const key = `${entry.competitionId}:${entry.competitorId}`;
    if (!unique.has(key)) unique.set(key, entry);
  }
  const competitors = Map.groupBy(unique.values(), (entry) => entry.competitorId);
  const competitions = Map.groupBy(unique.values(), (entry) => entry.competitionId);
  const canToggle = competitors.size > 1 && competitions.size > 1;
  const byCompetition =
    canToggle && override !== null ? override : competitions.size < competitors.size;
  const groups = [...(byCompetition ? competitions : competitors)].toSorted(
    ([, a], [, b]) =>
      (byCompetition
        ? (checkInTime(a) ?? '24:00').localeCompare(checkInTime(b) ?? '24:00')
        : 0) ||
      compareText(entryLabel(a[0]!, byCompetition), entryLabel(b[0]!, byCompetition)),
  );

  return (
    <div className="flex flex-col gap-1">
      <div
        className={cn(
          'text-sm font-semibold leading-tight',
          cards ? 'text-neutral-12' : 'text-green-11',
        )}
      >
        <CompetitionLink
          entry={event}
          className={cards ? undefined : 'text-green-11 hover:text-green-12'}
        >
          {event.competitionEventName}
        </CompetitionLink>
      </div>
      <div
        className={cn(
          'flex items-baseline justify-between gap-2 text-xs leading-tight',
          cards ? 'text-neutral-11' : 'text-green-11',
        )}
      >
        <span className="min-w-0 truncate">
          {[
            event.competitionEventLocation,
            cards && event.competitionDate && formatDateOnly(event.competitionDate),
          ]
            .filter(Boolean)
            .join(', ')}
        </span>
        {canToggle && (
          <button
            type="button"
            aria-label={
              byCompetition ? 'Seskupit podle účastníků' : 'Seskupit podle soutěží'
            }
            className="inline-flex shrink-0 items-center gap-1 rounded px-1 py-0.5 text-[11px] leading-tight text-green-11 hover:bg-green-4 focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-green-8"
            onClick={() => setOverride(!byCompetition)}
          >
            <ArrowLeftRight className="size-3" aria-hidden />
            {byCompetition ? 'Soutěže' : 'Účastníci'}
          </button>
        )}
      </div>
      {groups.map(([key, entries]) => (
        <article
          key={key}
          className={
            cards
              ? cardCls({ className: 'rounded-lg border-neutral-6 p-2 pb-1' })
              : 'border-t border-green-6 pt-2 first-of-type:border-t-0'
          }
        >
          <CompetitionGroupContent entries={entries} byCompetition={byCompetition} />
        </article>
      ))}
    </div>
  );
}

function CompetitionLabel({
  entry,
  competition,
  cancelled = entry.cancelled,
}: {
  entry: CompetitionEntry;
  competition: boolean;
  cancelled?: boolean;
}) {
  return competition ? (
    <CompetitionLink
      entry={entry}
      competitionId={entry.competitionExternalId}
      className={cn(
        'text-accent-11',
        (cancelled || entry.status === 'CANCELLED') && 'line-through',
      )}
      title={cancelled ? 'Odhlášená registrace' : undefined}
    >
      {entryLabel(entry, true)}
    </CompetitionLink>
  ) : (
    <span
      className={cancelled ? 'line-through' : undefined}
      title={cancelled ? 'Odhlášená registrace' : undefined}
    >
      {entry.competitorName}
    </span>
  );
}

function CompetitionGroupContent({
  entries,
  byCompetition,
}: {
  entries: readonly CompetitionEntry[];
  byCompetition: boolean;
}) {
  const checkInEnd = byCompetition && checkInTime(entries);
  const showLeadingColumn =
    !byCompetition || entries.some((x) => x.ranking !== undefined);
  return (
    <div
      className={cn(
        'grid gap-x-2 @max-[99px]/competition:grid-cols-1',
        showLeadingColumn
          ? 'grid-cols-[max-content_minmax(0,1fr)_max-content]'
          : 'grid-cols-[minmax(0,1fr)_max-content]',
      )}
    >
      <h5 className="col-span-full flex items-baseline gap-2 wrap-break-word pb-1 text-sm font-medium leading-tight text-neutral-12">
        {checkInEnd && (
          <span className="shrink-0 text-xs tabular-nums text-neutral-11">
            {checkInEnd.slice(0, 5)}
          </span>
        )}
        <span className="min-w-0">
          <CompetitionLabel
            entry={entries[0]!}
            competition={byCompetition}
            cancelled={entries.every((entry) => entry.cancelled)}
          />
        </span>
      </h5>
      {entries
        .toSorted(
          (a, b) =>
            Number(a.ranking === undefined) - Number(b.ranking === undefined) ||
            (a.ranking ?? Infinity) - (b.ranking ?? Infinity) ||
            Number(b.finalGain ?? 0) - Number(a.finalGain ?? 0) ||
            Number(b.pointGain ?? 0) - Number(a.pointGain ?? 0) ||
            (a.ranking === undefined
              ? (a.checkInEnd ?? '24:00').localeCompare(b.checkInEnd ?? '24:00')
              : 0) ||
            compareText(entryLabel(a, !byCompetition), entryLabel(b, !byCompetition)),
        )
        .map((entry) => {
          const result = entry.ranking !== undefined ? entry : null;
          const status = formatResultStatus(entry.completionStatus);
          return (
            <div
              key={entry.id}
              className="col-span-full grid grid-cols-subgrid items-baseline py-1.5 text-xs"
            >
              {showLeadingColumn && (
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
                    : !byCompetition && entry.checkInEnd?.slice(0, 5)}
                </div>
              )}
              <div className="min-w-0 font-semibold text-neutral-12">
                <CompetitionLabel entry={entry} competition={!byCompetition} />
              </div>
              {result && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-green-11">
                  {formatPointsAndFinals(result.pointGain, result.finalGain, 'gain')}
                </div>
              )}
              {status && <div className="col-span-full text-neutral-11">{status}</div>}
            </div>
          );
        })}
    </div>
  );
}

export function CompetitionLink({
  entry,
  competitionId,
  children,
  className,
  title,
}: {
  entry?: Pick<CompetitionEntry, 'federation' | 'competitionEventExternalId'>;
  competitionId?: string | null;
  children: React.ReactNode;
  className?: string;
  title?: string;
}) {
  if (!entry?.federation || !entry.competitionEventExternalId) return <>{children}</>;

  return (
    <Link
      href={`/federated/${entry.federation}/${entry.competitionEventExternalId}${competitionId ? `/${competitionId}` : ''}`}
      prefetch={false}
      className={cn('underline hover:text-accent-11', className)}
      title={title}
    >
      {children}
    </Link>
  );
}

function competitionDetails(competition: ParticipationCompetitionFragment) {
  return {
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
}

function competitionEntries(
  competitions: NonNullable<CompetitionParticipationsQuery['competitions']>['nodes'],
): CompetitionEntry[] {
  return competitions.flatMap((competition) => {
    const common = competitionDetails(competition);
    const entries = new Map<string, CompetitionEntry>();
    for (const entry of [...competition.entries, ...competition.results]) {
      entries.set(entry.competitorId, {
        ...entries.get(entry.competitorId),
        ...common,
        ...entry,
        id: `${competition.id}:${entry.competitorId}`,
        competitorName: entry.competitor?.name ?? entry.competitorId,
      });
    }
    return [...entries.values()];
  });
}

export function CompetitionHistory({
  participant,
}: {
  participant: CompetitionParticipantCondition;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionParticipationsDocument,
    variables: (offset): CompetitionParticipationsQueryVariables => ({
      participant,
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

export function CompetitionSuggestions({
  participant = {},
}: {
  participant?: CompetitionParticipantCondition;
}) {
  const [since] = React.useState(() => format(new Date(), 'yyyy-MM-dd'));
  const [mapView, setMapView] = React.useState<MapView>();
  const [showMap, setShowMap] = React.useState(true);
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionSuggestionEventsDocument,
    variables: (offset) => ({ participant, since, offset }),
    limit: 10,
    pageKey: 'suggestionPage',
    getTotal: (data) => data?.federationEvents?.totalCount,
  });
  const events = data?.federationEvents?.nodes ?? [];
  const [{ data: matchData, fetching: matchFetching, error: matchError }] = useQuery({
    query: CompetitionSuggestionsDocument,
    variables: { ...participant, since, eventIds: events.map((event) => event.id) },
    pause: fetching || !!error || events.length === 0,
  });

  const matches = (matchData?.competitionSuggestionsList ?? []).flatMap((row) => {
    if (!row.competition || (!row.couple && !row.person)) return [];
    const competitor = row.couple
      ? {
          key: `couple:${row.couple.id}`,
          name: [row.couple.man?.name, row.couple.woman?.name]
            .filter(Boolean)
            .join(' – '),
          href: `/pary/${row.couple.id}`,
        }
      : {
          key: `person:${row.person!.id}`,
          name: row.person!.name,
          href: `/clenove/${row.person!.id}`,
        };
    return [
      { ...competitionDetails(row.competition), id: row.competition.id, competitor },
    ];
  });
  const byEvent = Map.groupBy(matches, (match) => match.competitionEventId);
  const queryError = error ?? (events.length > 0 ? matchError : undefined);
  const hasCoordinates = events.some((x) => x.venueLat !== null && x.venueLng !== null);

  return (
    <div>
      {hasCoordinates && (
        <Checkbox
          name="suggestions-map"
          label="Mapa"
          checked={showMap}
          onChange={(event) => setShowMap(event.currentTarget.checked)}
          className="mb-2"
        />
      )}
      {queryError ? (
        <FormError error={queryError} />
      ) : fetching || (events.length > 0 && matchFetching) ? (
        <p role="status" className="text-sm text-neutral-11">
          Načítám...
        </p>
      ) : events.length > 0 ? (
        <div className="flex flex-col gap-3">
          {showMap && hasCoordinates && (
            <EventMap
              events={events}
              view={mapView}
              onViewChange={setMapView}
              getEventWeight={(x) => byEvent.get(x.id)?.length ?? 0}
              renderEventDetails={(event) => {
                const matches = byEvent.get(event.id) ?? [];
                return (
                  <DotSeparated className="text-xs text-neutral-11">
                    {formatCount(
                      new Set(matches.map((match) => match.competitor.key)).size,
                      { one: 'soutěžící', few: 'soutěžící', other: 'soutěžících' },
                    )}
                    {formatCount(new Set(matches.map((match) => match.id)).size, {
                      one: 'soutěž',
                      few: 'soutěže',
                      other: 'soutěží',
                    })}
                    {formatCount(matches.length, {
                      one: 'možný start',
                      few: 'možné starty',
                      other: 'možných startů',
                    })}
                  </DotSeparated>
                );
              }}
            />
          )}
          {events.map((event) => {
            const eventMatches = byEvent.get(event.id) ?? [];
            const competitors = [
              ...new Map(
                eventMatches.map((match) => [match.competitor.key, match.competitor]),
              ).values(),
            ];
            const competitions = [
              ...Map.groupBy(eventMatches, (match) => match.id).values(),
            ].toSorted(
              (a, b) =>
                compareText(a[0]!.competitionDate ?? '', b[0]!.competitionDate ?? '') ||
                compareText(a[0]!.checkInEnd ?? '24:00', b[0]!.checkInEnd ?? '24:00') ||
                compareText(entryLabel(a[0]!, true), entryLabel(b[0]!, true)),
            );
            return (
              <article
                key={event.id}
                className={cardCls({ className: 'rounded-lg p-3' })}
              >
                <h3 className="font-semibold">
                  <CompetitionLink
                    entry={{
                      federation: event.federation,
                      competitionEventExternalId: event.externalId,
                    }}
                  >
                    {event.name}
                  </CompetitionLink>
                </h3>

                <DotSeparated className="mb-2 flex text-sm text-neutral-11">
                  {[
                    event.startDate &&
                      [
                        formatDateOnly(event.startDate),
                        event.endDate &&
                          event.endDate !== event.startDate &&
                          formatDateOnly(event.endDate),
                      ]
                        .filter(Boolean)
                        .join(' – '),
                  ]}
                  {event.location || event.city}
                  <span className="font-medium text-accent-11">
                    {formatCount(competitions.length, {
                      one: 'soutěž',
                      few: 'soutěže',
                      other: 'soutěží',
                    })}
                  </span>
                  <EligibleCompetitors competitors={competitors} />
                </DotSeparated>

                <ul className="space-y-1 text-sm">
                  {competitions.map((competitionMatches) => {
                    const entry = competitionMatches[0]!;
                    return (
                      <li
                        key={entry.id}
                        className="flex items-baseline justify-between gap-3"
                      >
                        <CompetitionLink
                          entry={entry}
                          competitionId={entry.competitionExternalId}
                        >
                          {entryLabel(entry, true)}
                        </CompetitionLink>
                        <EligibleCompetitors
                          competitors={competitionMatches.map((x) => x.competitor)}
                        />
                      </li>
                    );
                  })}
                </ul>
              </article>
            );
          })}
        </div>
      ) : (
        <p className="text-sm text-neutral-11">Nebyly nalezeny žádné možné soutěže.</p>
      )}
      <Pagination {...pagination} />
    </div>
  );
}

function EligibleCompetitors({
  competitors,
}: {
  competitors: { key: string; name: string; href: string }[];
}) {
  return (
    <Popover>
      <PopoverTrigger asChild>
        <button
          type="button"
          className="shrink-0 font-medium text-accent-11 underline decoration-dotted underline-offset-2"
        >
          {formatCount(competitors.length, {
            one: 'soutěžící',
            few: 'soutěžící',
            other: 'soutěžících',
          })}
        </button>
      </PopoverTrigger>
      <PopoverContent align="start" className="max-h-80 overflow-y-auto text-sm">
        <h4 className="mb-2 pr-6 font-semibold">Možní soutěžící</h4>
        <ul className="space-y-1">
          {competitors
            .toSorted((a, b) => compareText(a.name, b.name))
            .map((competitor) => (
              <li key={competitor.key}>
                <Link
                  href={competitor.href}
                  prefetch={false}
                  className="text-accent-11 underline"
                >
                  {competitor.name}
                </Link>
              </li>
            ))}
        </ul>
      </PopoverContent>
    </Popover>
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
