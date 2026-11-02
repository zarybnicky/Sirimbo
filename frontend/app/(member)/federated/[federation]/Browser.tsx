'use client';

import Link from 'next/link';
import { cn } from '@/lib/cn';
import { compareNaturalText, compareText } from '@/lib/compare';
import { useEffect, useState } from 'react';
import { useQuery } from 'urql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Table, TableStatus } from '@/ui/Table';
import { SortButton } from '@/ui/SortButton';
import {
  parseAsArrayOf,
  parseAsBoolean,
  parseAsInteger,
  parseAsNumberLiteral,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { ListHeader } from '@/ui/ListHeader';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { FacetFilter } from '@/ui/FacetFilter';
import { buttonCls } from '@/ui/style';
import {
  compareAgeGroups,
  compareAgeRanges,
  compareCategories,
  compareClasses,
  compareDisciplines,
  formatAgeGroup,
  formatClass,
  formatCompetitorType,
  formatDiscipline,
} from '@/ui/csts';
import type { CompetitorType } from '@/graphql';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly, formatPointsAndFinals } from '@/ui/format';
import EventMap, { defaultMapViews, mapViews } from '@/ui/EventMap';
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

function facetLabel(key: string, value: string) {
  if (key === 'ageGroup') return formatAgeGroup(value) ?? value;
  if (key === 'discipline') return formatDiscipline(value) ?? value;
  if (key === 'class') return formatClass(value) ?? value;
  if (key === 'gender') return genderLabels[value]!;
  return key === 'type' ? (formatCompetitorType(value) ?? value) : value;
}

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

function useLastDefined<T>(value: T | null | undefined) {
  const [previous, setPrevious] = useState(value);
  if (value != null && value !== previous) setPrevious(value);
  return value ?? previous;
}

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

function Events(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const activeFacetCount =
    Number(filters.cancelled) +
    Number(filters.missing) +
    Number(filters.hasResults !== null);

  const [facets] = useQuery({
    query: FederationEventFacetsDocument,
    variables: { federation, includeCancelled: filters.cancelled },
  });
  const years = useLastDefined(facets.data?.federation?.eventFacets?.years) ?? [];

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
        since: new Date(Date.UTC(filters.year, (filters.month || 1) - 1, 1))
          .toISOString()
          .slice(0, 10),
        until: new Date(Date.UTC(filters.year, filters.month || 12, 1))
          .toISOString()
          .slice(0, 10),
      },
    }),
    limit: 300,
    getTotal: (data) => data.federation?.events.totalCount,
  });
  const counts = useLastDefined(result.data?.federation);
  const events = result.data?.federation?.events.nodes ?? [];
  const isPending = result.fetching || facets.fetching || searchPending;

  return (
    <div className="min-w-0">
      <PeriodPicker
        years={years.filter((y) => y !== null).toReversed()}
        year={filters.year}
        month={filters.month}
        onChange={(period) => setFilters({ ...period, page: null }, { history: 'push' })}
      />

      <ListHeader
        pagination={!(result.error ?? facets.error) ? result.pagination : undefined}
        search={filters.search}
        onSearchChange={(search) => setFilters({ search, page: null })}
        searchPlaceholder="Název, místo nebo ID…"
        activeFilterCount={activeFacetCount}
        onReset={
          filters.search !== '' || activeFacetCount > 0
            ? () =>
                setFilters({
                  search: null,
                  cancelled: null,
                  missing: null,
                  hasResults: null,
                  page: null,
                })
            : undefined
        }
        actions={
          <button
            type="button"
            aria-pressed={filters.map}
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setFilters({ map: !filters.map })}
          >
            Mapa
          </button>
        }
      >
        <div className="flex flex-wrap items-center gap-1 sm:col-span-2">
          {(
            [
              ['hasResults', true, 'S výsledky', counts?.withResults.totalCount],
              ['hasResults', false, 'Bez výsledků', counts?.withoutResults.totalCount],
              ['cancelled', true, 'Včetně zrušených', counts?.cancelledEvents.totalCount],
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
      </ListHeader>
      {filters.map && events.some((e) => e.venueLat !== null && e.venueLng !== null) && (
        <EventMap
          key={filters.shape === 'czechia' ? 'czechia' : 'world'}
          events={events}
          view={filters.shape}
          onViewChange={(shape) => setFilters({ shape }, { history: 'push' })}
        />
      )}

      <Table
        columns={['Datum', 'Soutěž', 'Místo', 'Země', 'Soutěží']}
        busy={isPending}
        error={result.error ?? facets.error}
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
            <td>{event.location || event.city}</td>
            <td>{event.country}</td>
            <td>{event.competitions.totalCount}</td>
          </tr>
        ))}
      </Table>
      {!(result.error ?? facets.error) && <Pagination {...result.pagination} />}
    </div>
  );
}

function People(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const activeFacetCount = peopleFacets.filter((x) => filters[x.key].length > 0).length;

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
  const facetOptions =
    useLastDefined(facetResult.data?.federation?.peopleFacetsList) ?? [];
  const ageGroups = useLastDefined(facetResult.data?.ageGroupsList) ?? [];
  const ageRanges = new Map(ageGroups.map((group) => [group.code, group.personAge]));

  const result = usePaginatedQuery({
    query: FederationPeopleDocument,
    pause: searchPending,
    variables: (offset) => ({ ...variables, offset }),
    limit: 50,
    getTotal: (data) => data.federation?.people.totalCount,
  });
  const isPending = result.fetching || searchPending || facetResult.fetching;
  return (
    <>
      <ListHeader
        pagination={!(result.error ?? facetResult.error) ? result.pagination : undefined}
        search={filters.search}
        onSearchChange={(search) => setFilters({ search, page: null })}
        searchPlaceholder="Jméno nebo ID…"
        activeFilterCount={activeFacetCount}
        defaultFiltersOpen={activeFacetCount > 0}
        onReset={
          filters.search !== '' || activeFacetCount > 0
            ? () =>
                setFilters({
                  search: null,
                  ageGroup: null,
                  gender: null,
                  nationality: null,
                  page: null,
                })
            : undefined
        }
      >
        {peopleFacets.map(({ key, label }) => {
          const counts = new Map(
            facetOptions
              .filter((x) => x.facet === key)
              .map((x) => [x.value!, x.count ?? undefined]),
          );
          const values = [...new Set([...counts.keys(), ...filters[key]])].toSorted(
            (a, b) => {
              if (a === 'unknown' || b === 'unknown')
                return Number(a === 'unknown') - Number(b === 'unknown');
              if (key === 'ageGroup')
                return (
                  compareAgeRanges(ageRanges.get(a) ?? null, ageRanges.get(b) ?? null) ||
                  compareNaturalText(a, b)
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
              value={filters[key]}
              options={values.map((value) => ({
                value,
                label: value === 'unknown' ? 'Neuvedeno' : facetLabel(key, value),
                count: counts.get(value),
              }))}
              onChange={(value) => setFilters({ [key]: value, page: null })}
            />
          );
        })}
      </ListHeader>
      <Table
        columns={['Jméno', 'Věk', 'Pohlaví', 'Národnost']}
        busy={isPending}
        error={result.error ?? facetResult.error}
        empty="Źádní lidé nenalezení"
      >
        {result.data?.federation?.people.nodes.map((row) => (
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
            <td>{formatAgeGroup(row.ageGroupDetails ?? row.ageGroup) || '–'}</td>
            <td>{genderLabels[row.gender?.toLowerCase() ?? 'unknown']}</td>
            <td>{row.nationality || '–'}</td>
          </tr>
        ))}
      </Table>
      {!(result.error ?? facetResult.error) && <Pagination {...result.pagination} />}
    </>
  );
}

function Competitors(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
  const today = new Date().toISOString().slice(0, 10);

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

  const facetOptions =
    useLastDefined(facetResult.data?.federation?.competitorFacetsList) ?? [];
  const ageGroups = useLastDefined(facetResult.data?.ageGroupsList) ?? [];

  const ageGroupsByCode = new Map(ageGroups.map((x) => [x.code, x]));
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
  const competitors = (item?.competitors.nodes ?? []).map((x) => ({
    ...x,
    categoryProgressesList: x.categoryProgressesList.filter(
      (p) =>
        p.category?.class !== 'Entry' ||
        Number(p.points) !== 0 ||
        p.domesticFinals + p.foreignFinals !== 0,
    ),
  }));
  const availableDisciplines = new Set(
    competitors.flatMap((x) =>
      x.categoryProgressesList.flatMap((p) =>
        p.category ? [p.category.discipline] : [],
      ),
    ),
  );
  const disciplines = [...new Set(['Standard', 'Latin', ...availableDisciplines])].filter(
    (x) => availableDisciplines.has(x),
  );
  return (
    <>
      <ListHeader
        pagination={!(result.error ?? facetResult.error) ? result.pagination : undefined}
        search={filters.search}
        onSearchChange={(search) => setFilters({ search, page: null })}
        searchPlaceholder="Jméno nebo ID…"
        activeFilterCount={activeFacetCount}
        defaultFiltersOpen={activeFacetCount > 0}
        onReset={
          filters.search !== '' || activeFacetCount > 0 || filters.competedThisYear
            ? () => setFilters({ ...clearedCompetitorFilters, search: null })
            : undefined
        }
        actions={
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
        }
      >
        {facets.map(({ key, label }) => {
          const counts = new Map(
            facetOptions
              .filter((x) => x.facet === key)
              .map((x) => [
                key === 'type' ? x.value!.toUpperCase() : x.value!,
                x.count ?? undefined,
              ]),
          );
          const values = [...new Set([...counts.keys(), ...filters[key]])].toSorted(
            (a, b) => {
              if (key === 'ageGroup') {
                return (
                  compareAgeGroups(ageGroupsByCode.get(a), ageGroupsByCode.get(b)) ||
                  compareNaturalText(a, b)
                );
              }
              if (key === 'class') {
                return compareClasses(a, b) || compareNaturalText(a, b);
              }
              if (key === 'discipline') {
                const discipline = compareDisciplines(a, b);
                if (discipline) return discipline;
              }
              return (
                (counts.get(b) ?? 0) - (counts.get(a) ?? 0) || compareNaturalText(a, b)
              );
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
                label: facetLabel(key, value),
                count: counts.get(value),
              }))}
              onChange={(value) => setFilters({ [key]: value, page: null })}
            />
          );
        })}
      </ListHeader>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['competitor', 'Typ', 'Věk', 'Poslední soutěž', ...disciplines].map(
                (column) => (
                  <th key={column} scope="col" className="px-2 py-1">
                    {['Typ', 'Věk', 'Poslední soutěž'].includes(column) ? (
                      column
                    ) : (
                      <SortButton
                        column={column}
                        value={{ column: filters.sort, descending: filters.descending }}
                        defaultDescending={column !== 'competitor'}
                        onChange={({ column, descending }) =>
                          setFilters({ sort: column, descending, page: null })
                        }
                      >
                        {column === 'competitor' ? 'Jméno' : formatDiscipline(column)}
                      </SortButton>
                    )}
                  </th>
                ),
              )}
            </tr>
          </thead>
          <tbody>
            <TableStatus
              colSpan={4 + disciplines.length}
              busy={isPending || facetResult.fetching}
              error={result.error ?? facetResult.error}
              empty={competitors.length === 0 ? 'No competitors found' : undefined}
            />
          </tbody>
          {competitors.map((row) => {
            const progressByAge = Object.groupBy(
              row.categoryProgressesList.toSorted((a, b) =>
                compareCategories(a.category, b.category),
              ),
              (p) => p.category?.ageGroup ?? '',
            );
            const ageGroups = Object.keys(progressByAge).toSorted((a, b) => {
              const left = progressByAge[a]?.[0]?.category?.ageGroupDetails;
              const right = progressByAge[b]?.[0]?.category?.ageGroupDetails;
              return compareAgeGroups(left, right) || compareNaturalText(a, b);
            });
            if (ageGroups.length === 0) ageGroups.push('');
            return (
              <tbody key={row.id} className="border-t border-neutral-4">
                {ageGroups.map((ageGroup, index) => (
                  <tr key={ageGroup}>
                    {index === 0 && (
                      <>
                        <th scope="rowgroup" rowSpan={ageGroups.length}>
                          <Link
                            className="text-accent-11 underline"
                            href={`/federated/${federation}/competitors/${row.externalId}`}
                            prefetch={false}
                          >
                            {row.name || row.id}
                          </Link>
                        </th>

                        <td rowSpan={ageGroups.length}>
                          {formatCompetitorType(row.competitorType)}
                        </td>
                      </>
                    )}
                    <td className="whitespace-nowrap">
                      {formatAgeGroup(
                        progressByAge[ageGroup]?.[0]?.category?.ageGroupDetails ??
                          ageGroup,
                      ) || '–'}
                    </td>

                    {index === 0 && (
                      <td rowSpan={ageGroups.length} className="whitespace-nowrap">
                        {row.latestResultAt ? formatDateOnly(row.latestResultAt) : ''}
                        {row.latestEntryAt && row.latestEntryAt > today && (
                          <div className="text-xs text-neutral-11">(+ budoucí)</div>
                        )}
                      </td>
                    )}

                    {disciplines.map((x) => (
                      <td key={x} className="whitespace-nowrap">
                        {(progressByAge[ageGroup] ?? [])
                          .filter((p) => p.category?.discipline === x)
                          .map((p) => (
                            <div key={p.categoryId}>
                              <span className="font-medium">
                                {formatClass(p.category?.class) ?? p.category?.class}
                              </span>{' '}
                              {formatPointsAndFinals(p)}
                            </div>
                          ))}
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            );
          })}
        </table>
      </div>
      {!(result.error ?? facetResult.error) && <Pagination {...result.pagination} />}
    </>
  );
}
