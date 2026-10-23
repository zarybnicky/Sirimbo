'use client';

import Link from 'next/link';
import { cn } from '@/lib/cn';
import { compareNaturalText, compareText } from '@/lib/compare';
import { type ReactNode, useEffect, useState } from 'react';
import { useQuery } from 'urql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Table } from '@/ui/Table';
import { ArrowDown, ArrowUpDown, ChevronDown, SlidersHorizontal } from 'lucide-react';
import {
  parseAsArrayOf,
  parseAsBoolean,
  parseAsInteger,
  parseAsNumberLiteral,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { TextField } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { FacetFilter } from '@/ui/FacetFilter';
import { buttonCls } from '@/ui/style';
import { compareAgeRanges, formatAgeGroup, formatClass } from '@/ui/csts';
import type { CompetitorType } from '@/graphql';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly } from '@/ui/format';
import EventMap, { defaultMapViews, mapViews } from '../EventMap';
import { PeriodPicker } from '../PeriodPicker';
import {
  FederationCompetitorFacetsDocument,
  FederationCompetitorsDocument,
  type FederationCompetitorsQueryVariables,
  FederationEventFacetsDocument,
  FederationEventsDocument,
  FederationPeopleDocument,
  FederationPeopleFacetsDocument,
} from '@/graphql/Federation';

const views = ['events', 'people', 'competitors'] as const;

const competitorTypes = [
  'COUPLE',
  'SOLO',
  'DUO',
  'TRIO',
  'TEAM',
  'GROUP',
  'FORMATION',
] as const satisfies readonly CompetitorType[];
const classOrder = [
  'Entry',
  'Novice',
  'Bronze',
  'Silver',
  'Gold',
  'E',
  'D',
  'C',
  'B',
  'A',
  'S',
  'M',
  'Open',
].toReversed();

const facets = [
  { key: 'type', label: 'Typ' },
  { key: 'class', label: 'Třída' },
  { key: 'discipline', label: 'Disciplína' },
  { key: 'ageGroup', label: 'Věk' },
] as const;

const peopleFacets = [
  { key: 'ageGroup', label: 'Věk' },
  { key: 'gender', label: 'Pohlaví' },
  { key: 'nationality', label: 'Národnost' },
] as const;

const genderLabels: Record<string, string> = {
  male: 'Muž',
  female: 'Žena',
  unknown: 'Neuvedeno',
};

const clearedCompetitorFilters = {
  type: null,
  ageGroup: null,
  class: null,
  discipline: null,
  competedThisYear: null,
  page: null,
};

function useBrowserState(federation: string) {
  const [filters, setFilters] = useQueryStates(
    {
      search: parseAsString.withDefault(''),
      month: parseAsNumberLiteral<number>([
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
      ]).withDefault(0),
      year: parseAsInteger.withDefault(new Date().getFullYear()),
      shape: parseAsStringLiteral(mapViews.map((s) => s.value)).withDefault(
        defaultMapViews[federation] ?? 'world',
      ),
      missing: parseAsBoolean.withDefault(false),
      cancelled: parseAsBoolean.withDefault(false),
      hasResults: parseAsBoolean,
      map: parseAsBoolean.withDefault(true),
      page: parseAsInteger,
      type: parseAsArrayOf(parseAsStringLiteral(competitorTypes)).withDefault([]),
      ageGroup: parseAsArrayOf(parseAsString).withDefault([]),
      gender: parseAsArrayOf(
        parseAsStringLiteral(['male', 'female', 'unknown'] as const),
      ).withDefault([]),
      nationality: parseAsArrayOf(parseAsString).withDefault([]),
      class: parseAsArrayOf(parseAsString).withDefault([]),
      discipline: parseAsArrayOf(parseAsString).withDefault([]),
      competedThisYear: parseAsBoolean.withDefault(false),
      sort: parseAsString.withDefault('competitor'),
      descending: parseAsBoolean.withDefault(false),
      view: parseAsStringLiteral(views).withDefault('events'),
    },
    { shallow: true, scroll: false },
  );
  const [search, setSearch] = useState(filters.search.trim());
  useEffect(() => {
    const timeout = setTimeout(() => setSearch(filters.search.trim()), 300);
    return () => clearTimeout(timeout);
  }, [filters.search]);

  return { filters, setFilters, search, searchPending: search !== filters.search.trim() };
}

type BrowserProps = { federation: string } & ReturnType<typeof useBrowserState>;

export function Browser({ federation }: { federation: string }) {
  const state = useBrowserState(federation);
  const props = { federation, ...state };
  return (
    <TabMenu
      selected={state.filters.view}
      onSelect={(view) => {
        const selected = views.find((item) => item === view);
        if (selected)
          void state.setFilters(
            {
              view: selected,
              search: null,
              sort: null,
              descending: null,
              ...clearedCompetitorFilters,
              gender: null,
              nationality: null,
            },
            { history: 'push' },
          );
      }}
    >
      <Tab id="events" title="Soutěže">
        <Events {...props} />
      </Tab>
      <Tab id="people" title="Lidé">
        <People key={federation} {...props} />
      </Tab>
      <Tab id="competitors" title="Soutěžící">
        <Competitors key={federation} {...props} />
      </Tab>
    </TabMenu>
  );
}

function Results({
  browser: { filters, setFilters, searchPending },
  result,
  controls,
  actions,
  searchLabel = 'Jméno nebo ID',
  children,
}: {
  browser: BrowserProps;
  result: Pick<ReturnType<typeof usePaginatedQuery>, 'fetching' | 'error' | 'pagination'>;
  controls?: ReactNode;
  actions?: ReactNode;
  searchLabel?: string;
  children: ReactNode;
}) {
  const { fetching, error, pagination } = result;
  const { total, page, limit } = pagination;
  const isPending = fetching || searchPending;
  return (
    <>
      <div className="flex flex-wrap items-center gap-2 py-2">
        <p
          role="status"
          className="whitespace-nowrap text-xs tabular-nums text-neutral-11"
        >
          {isPending
            ? 'Načítám…'
            : !children || error
              ? null
              : total === 0
                ? '0 / 0'
                : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
        </p>
        {actions}
        <TextField
          name="search"
          type="search"
          aria-label={searchLabel}
          placeholder={`${searchLabel}…`}
          className="min-w-40 flex-1 sm:max-w-64 ml-auto"
          inputClassName="border-accent-6 bg-accent-2 py-1.5 text-sm text-neutral-12 placeholder:text-neutral-10 focus:border-accent-8 focus:ring-accent-8"
          value={filters.search}
          onChange={(event) => setFilters({ search: event.target.value, page: null })}
        />
      </div>
      {controls}
      <FormError error={error} />
      {!error && children && (
        <>
          {children}
          <Pagination {...pagination} />
        </>
      )}
    </>
  );
}

function Events(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const activeFacetCount =
    Number(filters.cancelled) +
    Number(filters.missing) +
    Number(filters.hasResults !== null);
  const [showFilters, setShowFilters] = useState(true);

  const [facets] = useQuery({
    query: FederationEventFacetsDocument,
    variables: { federation, includeCancelled: filters.cancelled },
  });
  const [facetData, setFacetData] = useState(facets.data);
  if (facets.data !== undefined && facets.data !== facetData) {
    setFacetData(facets.data);
  }

  const result = usePaginatedQuery({
    query: FederationEventsDocument,
    pause: searchPending,
    variables: (offset) => ({
      federation,
      search,
      offset,
      includeCancelled: filters.cancelled,
      hasResults: filters.hasResults,
      hasCoordinates: filters.missing ? false : null,
      date: {
        since: new Date(Date.UTC(filters.year, (filters.month || 1) - 1, 1)).toISOString().slice(0, 10),
        until: new Date(Date.UTC(filters.year, filters.month || 12, 1)).toISOString().slice(0, 10),
      },
    }),
    limit: 300,
    getTotal: (data) => data.federation?.events.totalCount,
  });
  const [counts, setCounts] = useState(result.data?.federation);
  if (result.data !== undefined && result.data.federation !== counts) {
    setCounts(result.data.federation);
  }
  const events = result.data?.federation?.events.nodes ?? [];
  const isPending = result.fetching || facets.fetching || searchPending;

  return (
    <div className="min-w-0">
      <PeriodPicker
        years={(facetData?.federation?.eventFacets?.years ?? [])
          .filter((y) => y !== null)
          .toReversed()}
        year={filters.year}
        month={filters.month}
        onChange={(period) => setFilters({ ...period, page: null }, { history: 'push' })}
      />

      <Results
        browser={props}
        result={{ ...result, fetching: isPending, error: result.error ?? facets.error }}
        searchLabel="Název, místo nebo ID"
        actions={
          <>
            <button
              type="button"
              aria-expanded={showFilters}
              aria-controls="event-facets"
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
              onClick={() => setShowFilters((visible) => !visible)}
            >
              <SlidersHorizontal className="size-3" aria-hidden />
              Filtry{activeFacetCount > 0 && ` (${activeFacetCount})`}
              <ChevronDown
                className={cn('size-3', showFilters && 'rotate-180')}
                aria-hidden
              />
            </button>

            <button
              type="button"
              aria-pressed={filters.map}
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
              onClick={() => setFilters({ map: !filters.map })}
            >
              Mapa
            </button>

            {(filters.search !== '' || activeFacetCount > 0) && (
              <button
                type="button"
                className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
                onClick={() =>
                  setFilters({
                    search: null,
                    cancelled: null,
                    missing: null,
                    hasResults: null,
                    page: null,
                  })
                }
              >
                Zrušit filtry
              </button>
            )}
          </>
        }
        controls={
          <div
            id="event-facets"
            className={cn(
              'mb-3 flex-wrap items-center gap-1',
              showFilters ? 'flex' : 'hidden',
            )}
          >
            {(
              [
                ['hasResults', true, 'S výsledky', counts?.withResults.totalCount],
                ['hasResults', false, 'Bez výsledků', counts?.withoutResults.totalCount],
                [
                  'cancelled',
                  true,
                  'Včetně zrušených',
                  counts?.cancelledEvents.totalCount,
                ],
                ['missing', true, 'Bez souřadnic', counts?.missingCoordinates.totalCount],
              ] as const
            ).map(([key, value, label, count]) => (
              <button
                key={`${key}-${value}`}
                type="button"
                aria-pressed={filters[key] === value}
                className={buttonCls({
                  display: 'compact',
                  variant: 'facet',
                  size: 'sm',
                })}
                onClick={() =>
                  setFilters({ [key]: filters[key] === value ? null : value, page: null })
                }
              >
                {label}
                <span className="tabular-nums opacity-75">{count ?? '…'}</span>
              </button>
            ))}
          </div>
        }
      >
        {filters.map &&
          events.some((e) => e.venueLat !== null && e.venueLng !== null) && (
            <EventMap
              key={filters.shape === 'czechia' ? 'czechia' : 'world'}
              events={events}
              view={filters.shape}
              onViewChange={(shape) => setFilters({ shape }, { history: 'push' })}
              federation={federation}
            />
          )}

        <Table
          columns={['Datum', 'Soutěž', 'Místo', 'Země', 'Soutěží']}
          busy={isPending}
          empty="Žádné události"
        >
          {events.map((event) => (
            <tr key={event.id} className="border-t border-neutral-4">
              <td className="whitespace-nowrap">
                {formatDateOnly(event.startDate)}
                {event.endDate && event.endDate !== event.startDate && (
                  <> – {formatDateOnly(event.endDate)}</>
                )}
              </td>
              <th scope="row">
                <Link
                  className={cn(
                    'text-accent-11',
                    event.status === 'CANCELLED' ? 'line-through' : 'underline',
                  )}
                  href={`/federated/${federation}/${event.externalId}`}
                  prefetch={false}
                >
                  {event.name || event.externalId}
                </Link>
                {event.status === 'CANCELLED' && (
                  <div className="text-xs text-neutral-11">Zrušeno</div>
                )}
              </th>
              <td>
                <div>{event.location || event.city}</div>
              </td>
              <td>{event.country}</td>
              <td>{event.competitions.totalCount}</td>
            </tr>
          ))}
        </Table>
      </Results>
    </div>
  );
}

function People(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const activeFacetCount = peopleFacets.filter((x) => filters[x.key].length > 0).length;
  const [showFilters, setShowFilters] = useState(activeFacetCount > 0);

  const variables = {
    federation,
    search,
    ageGroups: filters.ageGroup,
    genders: filters.gender,
    nationalities: filters.nationality,
  };

  const [facetResult] = useQuery({
    query: FederationPeopleFacetsDocument,
    variables,
    pause: searchPending,
  });
  const [facetData, setFacetData] = useState(facetResult.data);
  if (facetResult.data !== undefined && facetResult.data !== facetData) {
    setFacetData(facetResult.data);
  }
  const facetOptions = facetData?.federation?.peopleFacetsList ?? [];

  const ageRanges = new Map(
    (facetData?.ageGroupsList ?? []).map((group) => [group.code, group.personAge]),
  );
  const result = usePaginatedQuery({
    query: FederationPeopleDocument,
    pause: searchPending,
    variables: (offset) => ({ ...variables, offset }),
    limit: 50,
    getTotal: (data) => data.federation?.people.totalCount,
  });
  const item = result.data?.federation;
  const isPending = result.fetching || searchPending;
  return (
    <Results
      browser={props}
      result={{
        ...result,
        fetching: result.fetching || facetResult.fetching,
        error: result.error ?? facetResult.error,
      }}
      controls={
        <div
          id="people-facets"
          className={cn(
            'mb-3 w-full justify-items-start gap-2',
            showFilters ? 'grid' : 'hidden',
          )}
        >
          {peopleFacets.map(({ key, label }) => {
            const counts = new Map(
              facetOptions
                .filter((option) => option.facet === key)
                .map((option) => [option.value!, option.count ?? 0]),
            );
            const values = [...new Set([...counts.keys(), ...filters[key]])].toSorted(
              (a, b) => {
                if (a === 'unknown' || b === 'unknown')
                  return Number(a === 'unknown') - Number(b === 'unknown');
                if (key === 'ageGroup')
                  return (
                    compareAgeRanges(
                      ageRanges.get(a) ?? null,
                      ageRanges.get(b) ?? null,
                    ) || compareNaturalText(a, b)
                  );
                return (counts.get(b) ?? 0) - (counts.get(a) ?? 0) || compareText(a, b);
              },
            );
            return (
              <FacetFilter
                key={key}
                label={label}
                inline
                maxVisibleOptions={key === 'nationality' ? 5 : undefined}
                moreLabel="Další"
                clearLabel="Zrušit výběr"
                value={filters[key]}
                options={values.map((value) => ({
                  value,
                  label:
                    value === 'unknown'
                      ? 'Neuvedeno'
                      : key === 'ageGroup'
                        ? (formatAgeGroup(value) ?? value)
                        : key === 'gender'
                          ? genderLabels[value]!
                          : value,
                  count: counts.get(value) ?? 0,
                }))}
                onChange={(value) => setFilters({ [key]: value, page: null })}
              />
            );
          })}
        </div>
      }
      actions={
        <>
          <button
            type="button"
            aria-expanded={showFilters}
            aria-controls="people-facets"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setShowFilters((visible) => !visible)}
          >
            <SlidersHorizontal className="size-3" aria-hidden />
            Filtry{activeFacetCount > 0 && ` (${activeFacetCount})`}
            <ChevronDown
              className={cn('size-3', showFilters && 'rotate-180')}
              aria-hidden
            />
          </button>
          {(filters.search !== '' || activeFacetCount > 0) && (
            <button
              type="button"
              className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
              onClick={() =>
                setFilters({
                  search: null,
                  ageGroup: null,
                  gender: null,
                  nationality: null,
                  page: null,
                })
              }
            >
              Zrušit filtry
            </button>
          )}
        </>
      }
    >
      {item && (
        <Table
          columns={['Jméno', 'Věk', 'Pohlaví', 'Národnost']}
          busy={isPending}
          empty="Źádní lidé nenalezení"
        >
          {item.people.nodes.map((row) => (
            <tr key={row.id} className="border-t border-neutral-4">
              <th scope="row">
                <Link
                  className="text-accent-11 underline"
                  href={`/federated/${federation}/people/${row.externalId}`}
                  prefetch={false}
                >
                  {row.name ||
                    [row.firstName, row.lastName].filter(Boolean).join(' ') ||
                    row.id}
                </Link>
              </th>
              <td>
                {formatAgeGroup(row.ageGroupDetails?.code, row.ageGroupDetails?.name) ||
                  row.ageGroup ||
                  '–'}
              </td>
              <td>{genderLabels[row.gender?.toLowerCase() ?? 'unknown']}</td>
              <td>{row.nationality || '–'}</td>
            </tr>
          ))}
        </Table>
      )}
    </Results>
  );
}

function Competitors(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const today = new Date().toISOString().slice(0, 10);
  const [showFilters, setShowFilters] = useState(() =>
    facets.some(({ key }) => filters[key].length > 0),
  );
  const activeFacetCount = facets.filter(({ key }) => filters[key].length > 0).length;
  const filterVariables = {
    federation,
    search,
    competitorTypes: filters.type,
    ageGroups: filters.ageGroup,
    classes: filters.class,
    disciplines: filters.discipline,
    competedSince: filters.competedThisYear ? `${new Date().getFullYear()}-01-01` : null,
  };
  const [facetResult] = useQuery({
    query: FederationCompetitorFacetsDocument,
    variables: filterVariables,
    pause: searchPending,
    requestPolicy: 'cache-first',
  });

  const fetchedFacets = facetResult.data?.federation?.competitorFacetsList;
  const [facetOptions, setFacetOptions] = useState(fetchedFacets ?? []);
  if (
    facetResult.data !== undefined &&
    fetchedFacets !== facetOptions &&
    !!fetchedFacets?.length
  ) {
    setFacetOptions(fetchedFacets ?? []);
  }

  const ageRanges = new Map(
    (facetResult.data?.ageGroupsList ?? []).map((group) => [
      group.code,
      group.coupleRules[0]?.olderAge ?? group.age,
    ]),
  );
  const result = usePaginatedQuery({
    query: FederationCompetitorsDocument,
    pause: searchPending,
    variables: (offset): FederationCompetitorsQueryVariables => ({
      ...filterVariables,
      competitorOrder:
        filters.sort === 'competitor'
          ? [filters.descending ? 'NAME_DESC' : 'NAME_ASC', 'EXTERNAL_ID_ASC']
          : ['NATURAL'],
      orderByCategory:
        filters.sort === 'competitor'
          ? null
          : {
              discipline: filters.sort,
              descending: filters.descending,
            },
      offset,
    }),
    limit: 50,
    getTotal: (data) => data.federation?.competitors.totalCount,
  });
  const item = result.data?.federation;
  const isPending = result.fetching || searchPending;
  const competitors = (item?.competitors.nodes ?? []).map((competitor) => ({
    ...competitor,
    categoryProgressesList: competitor.categoryProgressesList.filter(
      (p) =>
        p.category?.class !== 'Entry' ||
        Number(p.points) !== 0 ||
        p.domesticFinals + p.foreignFinals !== 0,
    ),
  }));
  const availableDisciplines = new Set(
    competitors.flatMap((competitor) =>
      competitor.categoryProgressesList.flatMap((p) =>
        p.category ? [p.category.discipline] : [],
      ),
    ),
  );
  const disciplines = [...new Set(['Standard', 'Latin', ...availableDisciplines])].filter(
    (x) => availableDisciplines.has(x),
  );
  return (
    <Results
      browser={props}
      result={{
        ...result,
        fetching: result.fetching || facetResult.fetching,
        error: result.error ?? facetResult.error,
      }}
      controls={
        <>
          <div
            id="competitor-facets"
            className={cn(
              'mb-3 w-full justify-items-start gap-2',
              showFilters ? 'grid' : 'hidden',
            )}
          >
            {facets.map(({ key, label }) => {
              const counts = new Map(
                facetOptions
                  .filter((option) => option.facet === key)
                  .map((option) => [
                    key === 'type' ? option.value!.toUpperCase() : option.value!,
                    option.count ?? 0,
                  ]),
              );
              const values = [...new Set([...counts.keys(), ...filters[key]])].toSorted(
                (a, b) => {
                  if (key === 'ageGroup') {
                    return (
                      compareAgeRanges(ageRanges.get(a), ageRanges.get(b)) || compareNaturalText(a, b)
                    );
                  }
                  if (key === 'class') {
                    const aRank = classOrder.indexOf(a);
                    const bRank = classOrder.indexOf(b);
                    return (
                      (aRank === -1 ? classOrder.length : aRank) -
                        (bRank === -1 ? classOrder.length : bRank) || compareNaturalText(a, b)
                    );
                  }
                  if (key === 'discipline') {
                    const aRank = a === 'Standard' ? 0 : a === 'Latin' ? 1 : 2;
                    const bRank = b === 'Standard' ? 0 : b === 'Latin' ? 1 : 2;
                    if (aRank !== bRank) return aRank - bRank;
                  }
                  return (counts.get(b) ?? 0) - (counts.get(a) ?? 0) || compareNaturalText(a, b);
                },
              );
              if (values.length === 0) return null;
              return (
                <FacetFilter
                  key={key}
                  label={label}
                  inline
                  value={filters[key]}
                  options={values.map((value) => ({
                    value,
                    label:
                      key === 'type'
                        ? value.toLowerCase()
                        : key === 'ageGroup'
                          ? (formatAgeGroup(value) ?? value)
                          : key === 'class'
                            ? (formatClass(value) ?? value)
                            : value,
                    count: counts.get(value) ?? 0,
                  }))}
                  onChange={(value) => setFilters({ [key]: value, page: null })}
                />
              );
            })}
          </div>
        </>
      }
      actions={
        <>
          <button
            type="button"
            aria-expanded={showFilters}
            aria-controls="competitor-facets"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setShowFilters((visible) => !visible)}
          >
            <SlidersHorizontal className="size-3" aria-hidden />
            Filtry{activeFacetCount > 0 && ` (${activeFacetCount})`}
            <ChevronDown
              className={cn('size-3', showFilters && 'rotate-180')}
              aria-hidden
            />
          </button>
          {(filters.search !== '' ||
            filters.competedThisYear ||
            facets.some(({ key }) => filters[key].length > 0)) && (
            <button
              type="button"
              className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
              onClick={() => setFilters({ ...clearedCompetitorFilters, search: null })}
            >
              Zrušit filtry
            </button>
          )}
          <div className="flex-1" />
          <button
            type="button"
            aria-pressed={filters.competedThisYear}
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() =>
              setFilters({ competedThisYear: !filters.competedThisYear, page: null })
            }
          >
            Soutěžil tento rok
          </button>
        </>
      }
    >
      {item && (
        <div className="overflow-x-auto" aria-busy={isPending}>
          <table className="w-full text-left text-sm tabular-nums">
            <thead>
              <tr>
                {['Jméno', 'Typ', 'Věk', 'Poslední soutěž', ...disciplines].map(
                  (label) => (
                    <th
                      key={label}
                      scope="col"
                      className="px-2 py-1"
                      aria-sort={
                        filters.sort !== label
                          ? undefined
                          : filters.descending
                            ? 'descending'
                            : 'ascending'
                      }
                    >
                      {['Typ', 'Věk', 'Poslední soutěž'].includes(label) ? (
                        label
                      ) : (
                        <button
                          type="button"
                          className="inline-flex items-center gap-1 text-accent-11"
                          onClick={() =>
                            setFilters({
                              sort: label,
                              descending:
                                filters.sort === label
                                  ? !filters.descending
                                  : label !== 'competitor',
                              page: null,
                            })
                          }
                        >
                          {label}
                          {filters.sort === label ? (
                            <ArrowDown
                              className={cn(
                                'size-4',
                                !filters.descending && 'rotate-180',
                              )}
                              aria-hidden
                            />
                          ) : (
                            <ArrowUpDown className="size-4" aria-hidden />
                          )}
                        </button>
                      )}
                    </th>
                  ),
                )}
              </tr>
            </thead>
            {competitors.map((row) => {
              const progressByAge = Object.groupBy(
                row.categoryProgressesList
                  .toSorted((a, b) =>
                    (a.category?.class ?? '').localeCompare(b.category?.class ?? ''),
                  ),
                (p) => p.category?.ageGroup ?? '',
              );
              const ageGroups = Object.keys(progressByAge).toSorted((a, b) => {
                const left = progressByAge[a]?.[0]?.category?.ageGroupDetails;
                const right = progressByAge[b]?.[0]?.category?.ageGroupDetails;
                return compareAgeRanges(
                  left?.coupleRules[0]?.olderAge ?? left?.age ?? null,
                  right?.coupleRules[0]?.olderAge ?? right?.age ?? null,
                );
              });
              if (ageGroups.length === 0) ageGroups.push('');
              return (
                <tbody key={row.id} className="border-t border-neutral-4">
                  {ageGroups.map((ageGroup, index) => (
                    <tr key={ageGroup}>
                      {index === 0 && (
                        <>
                          <th
                            scope="rowgroup"
                            rowSpan={ageGroups.length}
                            className="px-2 py-1 font-normal align-top"
                          >
                            <Link
                              className="text-accent-11 underline"
                              href={`/federated/${federation}/competitors/${row.externalId}`}
                              prefetch={false}
                            >
                              {row.name || row.id}
                            </Link>
                          </th>

                          <td rowSpan={ageGroups.length} className="px-2 py-1 align-top">
                            {row.competitorType.toLowerCase()}
                          </td>
                        </>
                      )}
                      <td className="px-2 py-1 whitespace-nowrap align-top">
                        {formatAgeGroup(ageGroup) || '–'}
                      </td>

                      {index === 0 && (
                        <td
                          rowSpan={ageGroups.length}
                          className="px-2 py-1 whitespace-nowrap align-top"
                        >
                          {row.latestResultAt ? formatDateOnly(row.latestResultAt) : ''}
                          {row.latestEntryAt && row.latestEntryAt > today && (
                            <div className="text-xs text-neutral-11">
                              (+ budoucí)
                            </div>
                          )}
                        </td>
                      )}

                      {disciplines.map((x) => {
                        const entries = (progressByAge[ageGroup] ?? []).filter(
                          (p) => p.category?.discipline === x,
                        );
                        return (
                          <td key={x} className="px-2 py-1 align-top whitespace-nowrap">
                            {entries.length > 0
                              ? entries.map((p) => (
                                  <div key={p.categoryId}>
                                    <span className="font-medium">
                                      {formatClass(p.category?.class) ??
                                        p.category?.class}
                                    </span>{' '}
                                    {Number(p.points)}F
                                    {p.domesticFinals + p.foreignFinals}
                                  </div>
                                ))
                              : '–'}
                          </td>
                        );
                      })}
                    </tr>
                  ))}
                </tbody>
              );
            })}
            {competitors.length === 0 && (
              <tbody>
                <tr>
                  <td colSpan={4 + disciplines.length} className="px-2 py-1">
                    No competitors found
                  </td>
                </tr>
              </tbody>
            )}
          </table>
        </div>
      )}
    </Results>
  );
}
