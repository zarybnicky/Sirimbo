'use client';

import {
  parseAsArrayOf,
  parseAsBoolean,
  parseAsInteger,
  parseAsString,
  useQueryStates,
} from 'nuqs';
import { useQuery } from 'urql';
import type { CompetitionParticipantCondition } from '@/graphql';
import { CohortListDocument } from '@/graphql/Cohorts';
import { CoupleListDocument } from '@/graphql/Memberships';
import { PersonOptionsDocument } from '@/graphql/Person';
import { FacetFilter } from '@/ui/FacetFilter';
import { FormError } from '@/ui/form';

import dynamic from 'next/dynamic';
import type { MapView } from '@/ui/EventMap';
import { DotSeparated } from '@/ui/DotSeparated';
import {
  CompetitionSuggestionEventsDocument,
  CompetitionSuggestionsDocument,
} from '@/graphql/CompetitionParticipation';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { Pagination } from '@/ui/Pagination';
import { compareCategories, formatCategoryName } from '@/ui/csts';
import { compareText } from '@/lib/compare';
import { formatCount, formatDateOnly } from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { ChevronLeft } from 'lucide-react';
import * as Collapsible from '@radix-ui/react-collapsible';
import Link from 'next/link';
import * as React from 'react';
import { CompetitionLink } from '@/ui/Competitions';

const EventMap = dynamic(() => import('@/ui/EventMap'));

export function CompetitionPlanning() {
  const [since] = React.useState(() => new Date().toISOString().slice(0, 10));
  const [mapView, setMapView] = React.useState<MapView>();
  const [showMap, setShowMap] = React.useState(true);
  const [filters, setFilters] = useQueryStates(
    {
      personIds: parseAsArrayOf(parseAsString).withDefault([]),
      coupleId: parseAsString,
      cohortIds: parseAsArrayOf(parseAsString).withDefault([]),
      registered: parseAsBoolean.withDefault(false),
      page: parseAsInteger,
    },
    { shallow: true, scroll: false },
  );
  const [people] = useQuery({ query: PersonOptionsDocument });
  const [couples] = useQuery({ query: CoupleListDocument });
  const [cohorts] = useQuery({ query: CohortListDocument });
  const competitor: CompetitionParticipantCondition = {
    personIds: filters.personIds.length > 0 ? filters.personIds : null,
    coupleId: filters.coupleId,
    cohortIds: filters.cohortIds.length > 0 ? filters.cohortIds : null,
  };

  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionSuggestionEventsDocument,
    variables: (offset) => ({
      competitor,
      since,
      offset,
      registeredOnly: filters.registered,
    }),
    limit: 50,
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
  const plannedEvents = events.map((event) => {
    const eventMatches = byEvent.get(event.id) ?? [];
    const competitors = new Map<string, (typeof matches)[number]>();
    const competitions = new Map<
      string,
      {
        competition: (typeof matches)[number]['competition'];
        competitors: typeof matches;
      }
    >();
    for (const match of eventMatches) {
      if (!competitors.has(match.href) || match.status === 'registered') {
        competitors.set(match.href, match);
      }
      const group = competitions.get(match.competition.id);
      if (group) group.competitors.push(match);
      else
        competitions.set(match.competition.id, {
          competition: match.competition,
          competitors: [match],
        });
    }
    return {
      event,
      matchCount: eventMatches.length,
      competitors: [...competitors.values()],
      competitions: [...competitions.values()].toSorted((a, b) =>
        compareCategories(a.competition.category, b.competition.category),
      ),
    };
  });
  const summaries = new Map(plannedEvents.map((summary) => [summary.event.id, summary]));
  const queryError = error ?? (events.length > 0 ? matchError : undefined);
  const hasCoordinates = events.some((x) => x.venueLat !== null && x.venueLng !== null);
  const isPending = fetching || (events.length > 0 && matchFetching);
  const { total, page, limit } = pagination;

  return (
    <div>
      <div className="flex flex-wrap items-center gap-2">
        <FacetFilter
          label="Skupiny"
          value={filters.cohortIds}
          onChange={(cohortIds) =>
            void setFilters({ cohortIds, page: null }, { history: 'push' })
          }
          options={(cohorts.data?.cohortsList ?? []).map((x) => ({
            value: x.id,
            label: x.name,
          }))}
        />
        <FacetFilter
          label="Páry"
          value={filters.coupleId ? [filters.coupleId] : []}
          onChange={(ids) =>
            void setFilters({ coupleId: ids.at(-1) ?? null, page: null }, { history: 'push' })
          }
          options={(couples.data?.couplesList ?? []).map((x) => ({
            value: x.id,
            label: x.name,
          }))}
        />
        <FacetFilter
          label="Osoby"
          value={filters.personIds}
          onChange={(personIds) =>
            void setFilters({ personIds, page: null }, { history: 'push' })
          }
          options={(people.data?.people?.nodes ?? []).map((x) => ({
            value: x.id,
            label: x.name,
          }))}
        />
        <button
          type="button"
          aria-pressed={filters.registered}
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() =>
            setFilters(
              { registered: !filters.registered, page: null },
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
        <p className="ml-auto whitespace-nowrap text-xs tabular-nums text-neutral-11">
          {isPending
            ? 'Načítám…'
            : queryError
              ? null
              : total === 0
                ? '0 / 0'
                : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
        </p>
      </div>
      <FormError error={people.error ?? couples.error ?? cohorts.error} />
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
              getEventWeight={(event) => summaries.get(event.id)?.matchCount ?? 0}
              renderEventDetails={(event) => {
                const summary = summaries.get(event.id);
                if (!summary) return null;
                return (
                  <DotSeparated className="text-xs text-neutral-11">
                    {formatCount(summary.competitors.length, {
                      one: 'soutěžící',
                      few: 'soutěžící',
                      other: 'soutěžících',
                    })}
                    {formatCount(summary.competitions.length, {
                      one: 'soutěž',
                      few: 'soutěže',
                      other: 'soutěží',
                    })}
                  </DotSeparated>
                );
              }}
            />
          )}
          {plannedEvents.map(({ event, competitors, competitions }) => (
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
                      className="absolute inset-0 flex w-full cursor-pointer items-center justify-end rounded px-2 text-neutral-11 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8 [&[data-state=open]>svg]:-rotate-90"
                    >
                      <ChevronLeft className="size-5 transition-transform" />
                    </button>
                  </Collapsible.Trigger>
                </div>

                <Collapsible.Content>
                  <ul className="mt-2 space-y-1 text-sm">
                    {competitions.map(({ competition, competitors }) => (
                      <li
                        key={competition.id}
                        className="flex items-baseline justify-between gap-3"
                      >
                        <CompetitionLink
                          federation={competition.federation}
                          eventId={event.externalId}
                          competitionId={competition.externalId}
                        >
                          {formatCategoryName(competition)}
                        </CompetitionLink>
                        <EligibleCompetitors competitors={competitors} />
                      </li>
                    ))}
                  </ul>
                </Collapsible.Content>
              </article>
            </Collapsible.Root>
          ))}
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
