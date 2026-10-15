'use client';

import Link from 'next/link';
import { cn } from '@/lib/cn';
import { type ReactNode, useEffect, useState } from 'react';
import { useQuery } from 'urql';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Table } from '@/ui/Table';
import { z } from 'zod';
import { ArrowDown, ArrowUpDown } from 'lucide-react';
import {
  parseAsArrayOf,
  parseAsBoolean,
  parseAsInteger,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { TextField } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { FormError } from '@/ui/form';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { FacetFilter } from '@/ui/FacetFilter';
import { buttonCls } from '@/ui/style';
import { compareAgeRanges, formatAgeGroup, formatClass } from '@/ui/csts';
import type { CompetitorType } from '@/graphql';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly } from '@/ui/format';
import {
  FederationCompetitorFacetsDocument,
  FederationCompetitorsDocument,
  type FederationCompetitorsQueryVariables,
  FederationEventsDocument,
  FederationPeopleDocument,
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
  { key: 'type', label: 'Type' },
  { key: 'ageGroup', label: 'Age group' },
  { key: 'class', label: 'Class' },
  { key: 'discipline', label: 'Discipline' },
] as const;
const clearedCompetitorFilters = {
  type: null,
  ageGroup: null,
  class: null,
  discipline: null,
  competedThisYear: null,
  page: null,
};

function useBrowserState() {
  const [filters, setFilters] = useQueryStates(
    {
      search: parseAsString.withDefault(''),
      date: parseAsString.withDefault(''),
      page: parseAsInteger,
      type: parseAsArrayOf(parseAsStringLiteral(competitorTypes)).withDefault([]),
      ageGroup: parseAsArrayOf(parseAsString).withDefault([]),
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
  const state = useBrowserState();
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
              date: null,
              sort: null,
              descending: null,
              ...clearedCompetitorFilters,
            },
            { history: 'push' },
          );
      }}
    >
      <Tab id="events" title="Events">
        <Events {...props} />
      </Tab>
      <Tab id="people" title="People">
        <People {...props} />
      </Tab>
      <Tab id="competitors" title="Competitors">
        <Competitors {...props} />
      </Tab>
    </TabMenu>
  );
}

function Results({
  browser: { filters, setFilters, searchPending },
  result,
  invalidDate = false,
  controls,
  children,
}: {
  browser: BrowserProps;
  result: Pick<ReturnType<typeof usePaginatedQuery>, 'fetching' | 'error' | 'pagination'>;
  invalidDate?: boolean;
  controls?: ReactNode;
  children: ReactNode;
}) {
  const { fetching, error, pagination } = result;
  const { total, page, limit } = pagination;
  const isPending = fetching || searchPending;
  return (
    <>
      <div className="flex flex-wrap items-end gap-3 mb-2">
        <TextField
          name="search"
          label={filters.view === 'events' ? 'Name, location or event ID' : 'Name or ID'}
          value={filters.search}
          onChange={(event) => setFilters({ search: event.target.value, page: null })}
        />
        {controls}
      </div>
      <FormError error={invalidDate ? 'Invalid date' : error} />
      <p role="status" className="my-2 text-sm text-neutral-11">
        {isPending
          ? 'Loading…'
          : !children || invalidDate || error
            ? null
            : total === 0
              ? '0 results'
              : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} of ${total}`}
      </p>
      {!invalidDate && !error && children && (
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
  const invalidDate = !!filters.date && !z.iso.date().safeParse(filters.date).success;
  const result = usePaginatedQuery({
    query: FederationEventsDocument,
    pause: searchPending || invalidDate,
    variables: (offset) => ({ federation, search, onDate: filters.date || null, offset }),
    limit: 50,
    getTotal: (data) => data.federation?.events.totalCount,
  });
  const item = result.data?.federation;
  const isPending = result.fetching || searchPending;
  return (
    <Results
      browser={props}
      result={result}
      invalidDate={invalidDate}
      controls={
        <TextField
          name="date"
          label="Date"
          type="date"
          value={filters.date}
          onChange={(event) => setFilters({ date: event.target.value, page: null })}
        />
      }
    >
      {item && (
        <Table
          columns={['date', 'event', 'status', 'location', 'country', 'competitions']}
          busy={isPending}
          empty="No events found"
        >
          {item.events.nodes.map((event) => (
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
              </th>
              <td className="whitespace-nowrap">
                {event.status.toLowerCase().replaceAll('_', ' ')}
              </td>
              <td>{event.location || event.city}</td>
              <td>{event.country}</td>
              <td>{event.competitions.totalCount}</td>
            </tr>
          ))}
        </Table>
      )}
    </Results>
  );
}

function People(props: BrowserProps) {
  const { federation, search, searchPending } = props;
  const result = usePaginatedQuery({
    query: FederationPeopleDocument,
    pause: searchPending,
    variables: (offset) => ({ federation, search, offset }),
    limit: 50,
    getTotal: (data) => data.federation?.people.totalCount,
  });
  const item = result.data?.federation;
  const isPending = result.fetching || searchPending;
  return (
    <Results browser={props} result={result}>
      {item && (
        <Table columns={['person', 'age group']} busy={isPending} empty="No people found">
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
              <td>{formatAgeGroup(row.ageGroup) || '-'}</td>
            </tr>
          ))}
        </Table>
      )}
    </Results>
  );
}

function Competitors(props: BrowserProps) {
  const { federation, filters, setFilters, search, searchPending } = props;
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
  const facetOptions = facetResult.data?.federation?.competitorFacetsList ?? [];
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
  const availableDisciplines = new Set([
    ...facetOptions
      .filter((facet) => facet.facet === 'discipline')
      .map((facet) => facet.value!),
    ...(item?.competitors?.nodes ?? []).flatMap((competitor) =>
      competitor.categoryProgressesList.flatMap((p) =>
        p.category ? [p.category.discipline] : [],
      ),
    ),
  ]);
  const disciplines = [...new Set(['Standard', 'Latin', ...availableDisciplines])].filter(
    (discipline) => availableDisciplines.has(discipline),
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
          {' '}
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
              (a, b) =>
                key === 'ageGroup'
                  ? compareAgeRanges(ageRanges.get(a) ?? null, ageRanges.get(b) ?? null)
                  : a.localeCompare(b, 'en', { numeric: true }),
            );
            return (
              <FacetFilter
                key={key}
                label={label}
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
                onChange={(value) =>
                  setFilters({
                    [key]: value,
                    page: null,
                  })
                }
              />
            );
          })}
          <Checkbox
            name="competedThisYear"
            label="Competed this year"
            checked={filters.competedThisYear}
            onChange={(event) =>
              setFilters({
                competedThisYear: event.currentTarget.checked,
                page: null,
              })
            }
          />
          {(filters.competedThisYear ||
            facets.some(({ key }) => filters[key].length > 0)) && (
            <button
              type="button"
              className={buttonCls({ variant: 'outline', size: 'sm' })}
              onClick={() => setFilters(clearedCompetitorFilters)}
            >
              Clear filters
            </button>
          )}
        </>
      }
    >
      {item && (
        <div className="overflow-x-auto" aria-busy={isPending}>
          <table className="w-full text-left text-sm tabular-nums">
            <thead>
              <tr>
                {['competitor', 'type', 'age group', ...disciplines].map((label) => (
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
                    {label === 'type' || label === 'age group' ? (
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
                            className={cn('size-4', !filters.descending && 'rotate-180')}
                            aria-hidden
                          />
                        ) : (
                          <ArrowUpDown className="size-4" aria-hidden />
                        )}
                      </button>
                    )}
                  </th>
                ))}
              </tr>
            </thead>
            {item.competitors.nodes.map((row) => {
              const progressByAge = Object.groupBy(
                row.categoryProgressesList
                  .filter(
                    (p) =>
                      p.category?.class !== 'Entry' ||
                      Number(p.points) !== 0 ||
                      p.domesticFinals + p.foreignFinals !== 0,
                  )
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
                      {disciplines.map((discipline) => {
                        const entries = (progressByAge[ageGroup] ?? []).filter(
                          (p) => p.category?.discipline === discipline,
                        );
                        return (
                          <td
                            key={discipline}
                            className="px-2 py-1 align-top whitespace-nowrap"
                          >
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
            {item.competitors.nodes.length === 0 && (
              <tbody>
                <tr>
                  <td colSpan={3 + disciplines.length} className="px-2 py-1">
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
