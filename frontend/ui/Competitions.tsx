import dynamic from 'next/dynamic';
import { useAuth } from '@/lib/auth';
import type { MapView } from '@/ui/EventMap';
import { DotSeparated } from '@/ui/DotSeparated';
import {
  type CompetitionEntryFragment,
  type CompetitionFragment,
  type CompetitionResultFragment,
  CompetitionsDocument,
  type CompetitionsQuery,
  type CompetitionsQueryVariables,
  CompetitionSuggestionEventsDocument,
  CompetitionSuggestionsDocument,
} from '@/graphql/CompetitionParticipation';
import type { CompetitionParticipantCondition, OfficialRole } from '@/graphql';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { Pagination } from '@/ui/Pagination';
import { FormError } from '@/ui/form';
import { compareCategories, formatCategoryName } from '@/ui/csts';
import { cn } from '@/lib/cn';
import { compareText } from '@/lib/compare';
import {
  formatCount,
  formatDateOnly,
  formatOfficialRole,
  formatPointsAndFinals,
  formatResultStatus,
} from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { ArrowLeftRight, ChevronDown } from 'lucide-react';
import * as Collapsible from '@radix-ui/react-collapsible';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { parseAsBoolean, parseAsInteger, useQueryStates } from 'nuqs';
import { EntityLink } from '@/ui/EntityLink';

const EventMap = dynamic(() => import('@/ui/EventMap'));

type Competition = NonNullable<CompetitionsQuery['competitions']>['nodes'][number];
export type CompetitionEntry = CompetitionEntryFragment &
  Partial<CompetitionResultFragment> & { competition: Competition };

export type CompetitionOfficialAssignment = {
  date: string;
  federation: string;
  event: NonNullable<CompetitionFragment['event']>;
  role: OfficialRole;
  person: OfficialPersonFragment | null;
};

function entryLabel(entry: CompetitionEntry, competition: boolean) {
  return competition
    ? formatCategoryName(entry.competition)
    : (entry.competitor?.name ?? entry.competitorId);
}

export function groupCompetitionEvents(
  competitions: readonly Competition[] = [],
  officials: readonly CompetitionOfficialAssignment[] = [],
) {
  const entries = competitions.flatMap((comp) => {
    const entries = new Map<string, CompetitionEntry>();
    for (const entry of [...comp.entries, ...comp.results]) {
      entries.set(entry.competitorId, {
        cancelled: false,
        ...entries.get(entry.competitorId),
        ...entry,
        competition: comp,
      });
    }
    return [...entries.values()];
  });
  return [
    ...Map.groupBy([...entries, ...officials], (x) =>
      'role' in x ? x.date : x.competition.startDate,
    ),
  ].flatMap(([date, day]) =>
    [...Map.groupBy(day, (x) => ('role' in x ? x.event.id : x.competition.event?.id))]
      .map(([eventId, items]) => {
        const entries = items.filter((x): x is CompetitionEntry => !('role' in x));
        const officials = [
          ...new Map(
            items
              .filter((x) => 'role' in x)
              .map((x) => [`${x.role}:${x.person?.id}`, x] as const),
          ).values(),
        ];
        const event = entries[0]?.competition.event ?? officials[0]?.event;
        return {
          key: `${date ?? ''}:${eventId ?? ''}`,
          date,
          name: event?.name ?? '',
          location: event?.location || event?.city || '',
          entries,
          officials,
        };
      })
      .toSorted((a, b) => compareText(a.name, b.name)),
  );
}

export function CompetitionEventContent({
  entries,
  clubEntries = [],
  officials = [],
  cards = false,
  showHeader = true,
  collapsible = false,
  myCompetitorIds = [],
}: {
  entries: readonly CompetitionEntry[];
  clubEntries?: readonly CompetitionEntry[];
  officials?: readonly CompetitionOfficialAssignment[];
  cards?: boolean;
  showHeader?: boolean;
  collapsible?: boolean;
  myCompetitorIds?: readonly string[];
}) {
  const auth = useAuth();
  const [open, setOpen] = React.useState(false);
  const [override, setOverride] = React.useState<boolean | null>(null);
  const [expanded, setExpanded] = React.useState(false);
  const competition = entries[0]?.competition;
  const event = competition?.event ?? officials[0]?.event;
  const date = competition?.startDate ?? officials[0]?.date;
  if (!competition && !event) return null;
  const allEntries = expanded ? [...entries, ...clubEntries] : entries;
  const visible =
    collapsible && !open
      ? allEntries.filter((entry) => myCompetitorIds.includes(entry.competitorId))
      : allEntries;
  const visibleOfficials =
    collapsible && !open
      ? officials.filter(({ person }) =>
          person?.peopleList.some(({ id }) => auth.isMyPerson(id)),
        )
      : officials;
  const competitors = Map.groupBy(visible, (entry) => entry.competitorId);
  const competitions = Map.groupBy(visible, (entry) => entry.competitionId);
  const canToggle = competitors.size > 1 && competitions.size > 1;
  const byCompetition =
    canToggle && override !== null ? override : competitions.size < competitors.size;
  const groups = [...(byCompetition ? competitions : competitors)].toSorted(
    ([, a], [, b]) =>
      (byCompetition
        ? (a[0]!.competition.checkInEnd ?? '').localeCompare(
            b[0]!.competition.checkInEnd ?? '',
          )
        : 0) ||
      compareText(entryLabel(a[0]!, byCompetition), entryLabel(b[0]!, byCompetition)),
  );

  return (
    <div className="flex flex-col gap-1">
      <div
        className={cn(
          'relative flex flex-col gap-1',
          showHeader &&
            collapsible && [
              '-mx-3 -mt-3 rounded-lg p-3 hover:bg-green-4',
              visible.length === 0 &&
                visibleOfficials.length === 0 &&
                clubEntries.length === 0 &&
                '-mb-3',
            ],
        )}
      >
        {showHeader && collapsible && (
          <button
            type="button"
            onClick={() => setOpen(!open)}
            className="absolute inset-0 flex cursor-pointer items-center justify-end rounded-lg p-3 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-green-8"
          >
            <ChevronDown className={cn('size-4', open && 'rotate-180')} aria-hidden />
          </button>
        )}
        {showHeader && (
          <div
            className={cn(
              'flex items-start justify-between gap-2 text-sm font-semibold leading-tight',
              collapsible && 'relative pointer-events-none pr-6',
              cards ? 'text-neutral-12' : 'text-green-11',
            )}
          >
            <CompetitionLink
              federation={competition?.federation ?? officials[0]?.federation}
              eventId={event?.externalId}
              className={cn(
                collapsible && 'pointer-events-auto',
                !cards && 'text-green-11 hover:text-green-12',
              )}
            >
              {event?.name}
            </CompetitionLink>
          </div>
        )}
        {showHeader && (
          <div
            className={cn(
              'flex items-baseline justify-between gap-2 text-xs leading-tight',
              collapsible && 'relative pointer-events-none pr-6',
              cards ? 'text-neutral-11' : 'text-green-11',
            )}
          >
            {showHeader && (
              <span className="min-w-0 truncate">
                {[event?.location || event?.city, cards && date && formatDateOnly(date)]
                  .filter(Boolean)
                  .join(', ')}
              </span>
            )}
          </div>
        )}
      </div>
      {canToggle && (
        <div className="flex justify-end">
          <button
            type="button"
            className="inline-flex shrink-0 items-center gap-1 rounded px-1 py-0.5 text-[11px] leading-tight text-green-11 hover:bg-green-4 focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-green-8"
            onClick={() => setOverride(!byCompetition)}
          >
            <ArrowLeftRight className="size-3" aria-hidden />
            {byCompetition ? 'Soutěže' : 'Účastníci'}
          </button>
        </div>
      )}
      {groups.map(([key, entries]) => (
        <article
          key={key}
          className={
            cards
              ? cardCls({ className: 'rounded-lg border-neutral-6 p-2 pb-1' })
              : 'border-t border-green-6 first-of-type:border-t-0'
          }
        >
          <CompetitionGroupContent entries={entries} byCompetition={byCompetition} />
        </article>
      ))}
      {[...Map.groupBy(visibleOfficials, (x) => x.role)]
        .toSorted(([a], [b]) => a.localeCompare(b))
        .map(([role, items]) => {
          const people = items
            .flatMap(({ person }) => person ?? [])
            .toSorted((a, b) =>
              compareText(a.canonicalName ?? '', b.canonicalName ?? ''),
            );
          if (people.length === 0) return null;
          return (
            <div key={role} className="border-t border-green-6 pt-2 text-xs">
              <span className="font-medium">{formatOfficialRole(role)}: </span>
              {people.map((person, index) => (
                <React.Fragment key={person.id}>
                  {index > 0 && ', '}
                  <EntityLink type="federation-person" entity={person}>
                    {person.canonicalName}
                  </EntityLink>
                </React.Fragment>
              ))}
            </div>
          );
        })}
      {clubEntries.length > 0 && (
        <div className="mt-1 text-xs text-neutral-11">
          <button
            type="button"
            onClick={() => setExpanded(!expanded)}
            className="text-sm text-accent-11 underline underline-offset-2 hover:text-accent-12 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
          >
            Ostatní soutěžící
          </button>{' '}
          ({new Set(clubEntries.map((x) => x.competitorId)).size})
        </div>
      )}
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
      federation={entry.competition.federation}
      eventId={entry.competition.event?.externalId}
      competitionId={entry.competition.externalId}
      className={cn(
        'text-accent-11',
        (cancelled || entry.competition.status === 'CANCELLED') && 'line-through',
      )}
      title={cancelled ? 'Zrušená přihláška' : undefined}
    >
      {entryLabel(entry, true)}
    </CompetitionLink>
  ) : (
    <span
      className={cancelled ? 'line-through' : undefined}
      title={cancelled ? 'Zrušená přihláška' : undefined}
    >
      {entryLabel(entry, false)}
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
  const checkInEnd = byCompetition && entries[0]!.competition.checkInEnd;
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
            cancelled={entries.every((x) => x.cancelled)}
          />
        </span>
      </h5>
      {entries
        .toSorted(
          (a, b) =>
            (a.ranking ?? Infinity) - (b.ranking ?? Infinity) ||
            (a.ranking === undefined
              ? (a.competition.checkInEnd ?? '24:00').localeCompare(
                  b.competition.checkInEnd ?? '24:00',
                )
              : 0) ||
            compareText(entryLabel(a, !byCompetition), entryLabel(b, !byCompetition)),
        )
        .map((entry) => {
          const ranking = entry.ranking;
          const hasResult = ranking !== undefined;
          const status = formatResultStatus(entry.completionStatus);
          return (
            <div
              key={`${entry.competitionId}:${entry.competitorId}`}
              className="col-span-full grid grid-cols-subgrid items-baseline py-1.5 text-xs"
            >
              {showLeadingColumn && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-neutral-11">
                  {hasResult ? (
                    <>
                      <span className={ranking <= 3 ? 'text-accent-11' : ''}>
                        {ranking}
                        {entry.rankingTo && entry.rankingTo !== ranking
                          ? `–${entry.rankingTo}`
                          : ''}
                        .
                      </span>
                      {entry.competition.participantsTotal != null &&
                        ` z ${entry.competition.participantsTotal}`}
                    </>
                  ) : (
                    !byCompetition && entry.competition.checkInEnd?.slice(0, 5)
                  )}
                </div>
              )}
              <div className="min-w-0 font-semibold text-neutral-12">
                <CompetitionLabel entry={entry} competition={!byCompetition} />
              </div>
              {hasResult && (
                <div className="whitespace-nowrap font-semibold tabular-nums text-green-11">
                  {formatPointsAndFinals(entry, 'gain')}
                </div>
              )}
              {status && <div className="col-span-full text-neutral-11">{status}</div>}
            </div>
          );
        })}
    </div>
  );
}

function CompetitionLink({
  federation,
  eventId,
  competitionId,
  children,
  className,
  title,
}: {
  federation?: string | null;
  eventId?: string | null;
  competitionId?: string | null;
  children: React.ReactNode;
  className?: string;
  title?: string;
}) {
  if (!federation || !eventId) return <>{children}</>;

  return (
    <Link
      href={`/federated/${federation}/${eventId}${competitionId ? `/${competitionId}` : ''}`}
      prefetch={false}
      className={cn('underline hover:text-accent-11', className)}
      title={title}
    >
      {children}
    </Link>
  );
}

export function CompetitionHistory({
  competitor,
}: {
  competitor: CompetitionParticipantCondition;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionsDocument,
    variables: (offset): CompetitionsQueryVariables => ({
      competitor,
      orderBy: ['START_DATE_DESC', 'ID_DESC'],
      offset,
    }),
    limit: 50,
    pageKey: 'competitionPage',
    getTotal: (data) => data.competitions?.totalCount,
  });

  return (
    <>
      {error ? (
        <FormError error={error} />
      ) : fetching ? (
        <p className="text-sm text-neutral-11">Načítám...</p>
      ) : data?.competitions?.nodes.length ? (
        groupCompetitionEvents(data.competitions.nodes).map(({ key, entries }) => (
          <CompetitionEventContent key={key} entries={entries} cards />
        ))
      ) : (
        <p className="text-sm text-neutral-11">Žádné přihlášky ani výsledky soutěží.</p>
      )}
      <Pagination {...pagination} />
    </>
  );
}

export function CompetitionSuggestions({
  competitor = {},
}: {
  competitor?: CompetitionParticipantCondition;
}) {
  const [since] = React.useState(() => new Date().toISOString().slice(0, 10));
  const [mapView, setMapView] = React.useState<MapView>();
  const [showMap, setShowMap] = React.useState(true);
  const [filters, setFilters] = useQueryStates(
    {
      registered: parseAsBoolean.withDefault(false),
      suggestionPage: parseAsInteger,
    },
    { shallow: true, scroll: false },
  );
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionSuggestionEventsDocument,
    variables: (offset) => ({
      competitor,
      since,
      offset,
      registeredOnly: filters.registered,
    }),
    limit: 50,
    pageKey: 'suggestionPage',
    getTotal: (data) => data.federationEvents?.totalCount,
  });
  const events = data?.federationEvents?.nodes ?? [];
  const [{ data: matchData, fetching: matchFetching, error: matchError }] = useQuery({
    query: CompetitionSuggestionsDocument,
    variables: {
      ...competitor,
      since,
      eventIds: events.map((event) => event.id),
      registeredOnly: filters.registered,
    },
    pause: fetching || !!error || events.length === 0,
  });

  const matches = (matchData?.competitionSuggestionsList ?? []).flatMap((row) => {
    const competitor = row.couple ?? row.person;
    if (!row.competition || !competitor) return [];
    return [
      {
        competition: row.competition,
        status: row.status,
        name: competitor.name,
        href: `/${row.couple ? 'pary' : 'clenove'}/${competitor.id}`,
      },
    ];
  });
  const byEvent = Map.groupBy(matches, (match) => match.competition.eventId);
  const queryError = error ?? (events.length > 0 ? matchError : undefined);
  const hasCoordinates = events.some((x) => x.venueLat !== null && x.venueLng !== null);
  const isPending = fetching || (events.length > 0 && matchFetching);
  const { total, page, limit } = pagination;

  return (
    <div>
      <div className="flex flex-wrap items-center gap-2 py-2">
        <p className="whitespace-nowrap text-xs tabular-nums text-neutral-11">
          {isPending
            ? 'Načítám…'
            : queryError
              ? null
              : total === 0
                ? '0 / 0'
                : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
        </p>
        <button
          type="button"
          aria-pressed={filters.registered}
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() =>
            setFilters(
              { registered: !filters.registered, suggestionPage: null },
              { history: 'push' },
            )
          }
        >
          Již přihlášení
        </button>
        {hasCoordinates && (
          <button
            type="button"
            aria-pressed={showMap}
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setShowMap((x) => !x)}
          >
            Mapa
          </button>
        )}
      </div>
      {queryError ? (
        <FormError error={queryError} />
      ) : isPending ? (
        <p className="text-sm text-neutral-11">Načítám...</p>
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
                    {formatCount(new Set(matches.map((x) => x.href)).size, {
                      one: 'soutěžící',
                      few: 'soutěžící',
                      other: 'soutěžících',
                    })}
                    {formatCount(new Set(matches.map((x) => x.competition.id)).size, {
                      one: 'soutěž',
                      few: 'soutěže',
                      other: 'soutěží',
                    })}
                  </DotSeparated>
                );
              }}
            />
          )}
          {events.map((event) => {
            const eventMatches = byEvent.get(event.id) ?? [];
            const competitors = Map.groupBy(eventMatches, (x) => x.href)
              .values()
              .map((xs) => xs.find((x) => x.status === 'registered') ?? xs[0]!)
              .toArray();
            const competitions = Map.groupBy(eventMatches, (x) => x.competition.id)
              .values()
              .toArray()
              .toSorted((a, b) =>
                compareCategories(a[0]!.competition.category, b[0]!.competition.category),
              );
            return (
              <Collapsible.Root key={event.id} asChild>
                <article className={cardCls({ className: 'rounded-lg p-3' })}>
                  <div className="relative">
                    <div className="pointer-events-none relative z-10 min-w-0 pr-11 [&_a]:pointer-events-auto [&_button]:pointer-events-auto">
                      <h3 className="font-semibold">
                        <CompetitionLink
                          federation={event.federation}
                          eventId={event.externalId}
                        >
                          {event.name}
                        </CompetitionLink>
                      </h3>

                      <DotSeparated className="flex text-sm text-neutral-11">
                        {[
                          formatDateOnly(event.startDate),
                          event.endDate &&
                            event.endDate !== event.startDate &&
                            formatDateOnly(event.endDate),
                        ]
                          .filter(Boolean)
                          .join(' – ')}
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
                    </div>
                    <Collapsible.Trigger asChild>
                      <button
                        type="button"
                        className="absolute inset-0 flex w-full cursor-pointer items-center justify-end rounded px-2 text-neutral-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8 [&[data-state=open]>svg]:rotate-180"
                      >
                        <ChevronDown className="size-5 transition-transform" />
                      </button>
                    </Collapsible.Trigger>
                  </div>

                  <Collapsible.Content>
                    <ul className="mt-2 space-y-1 text-sm">
                      {competitions.map((matches) => {
                        const entry = matches[0]!;
                        return (
                          <li
                            key={entry.competition.id}
                            className="flex items-baseline justify-between gap-3"
                          >
                            <CompetitionLink
                              federation={entry.competition.federation}
                              eventId={event.externalId}
                              competitionId={entry.competition.externalId}
                            >
                              {formatCategoryName(entry.competition)}
                            </CompetitionLink>
                            <EligibleCompetitors competitors={matches} />
                          </li>
                        );
                      })}
                    </ul>
                  </Collapsible.Content>
                </article>
              </Collapsible.Root>
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
  competitors: { name: string; href: string; status: string | null }[];
}) {
  if (competitors.length === 0) return null;
  const registered = competitors.filter((x) => x.status === 'registered').length;
  return (
    <Popover>
      <PopoverTrigger asChild>
        <button
          type="button"
          className="shrink-0 font-medium text-accent-11 underline decoration-dotted underline-offset-2"
        >
          {competitors.length - registered > 0 &&
            formatCount(competitors.length - registered, {
              one: 'potenciální',
              few: 'potenciální',
              other: 'potenciálních',
            })}
          {registered > 0 && (
            <>
              {registered < competitors.length && ' · '}
              {formatCount(registered, {
                one: 'přihlášený',
                few: 'přihlášení',
                other: 'přihlášených',
              })}
            </>
          )}
        </button>
      </PopoverTrigger>
      <PopoverContent align="start" className="max-h-80 overflow-y-auto text-sm">
        <h4 className="mb-2 pr-6 font-semibold">Potenciální soutěžící</h4>
        <ul className="space-y-1">
          {competitors
            .toSorted((a, b) => compareText(a.name, b.name))
            .map((competitor) => (
              <li
                key={competitor.href}
                className="flex items-baseline justify-between gap-3"
              >
                <Link
                  href={competitor.href}
                  prefetch={false}
                  className="text-accent-11 underline"
                >
                  {competitor.name}
                </Link>
                {(competitor.status === 'registered' ||
                  competitor.status === 'cancelled') && (
                  <span className="shrink-0 text-xs text-neutral-11">
                    {competitor.status === 'registered' ? 'Přihlášeno' : 'Odhlášeno'}
                  </span>
                )}
              </li>
            ))}
        </ul>
      </PopoverContent>
    </Popover>
  );
}
