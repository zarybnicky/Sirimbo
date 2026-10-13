'use client';

import Link from 'next/link';
import { cn } from '@/lib/cn';
import { useEffect, useState } from 'react';
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
import { formatCstsClass } from '@/ui/csts';
import type { CompetitorType } from '@/graphql';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly } from '@/ui/format';
import { FederationDocument, type FederationQueryVariables } from '@/graphql/Federation';

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

export function Browser({ federation }: { federation: string }) {
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

  const { view, date } = filters;
  const searchPending = search !== filters.search.trim();
  const invalidDate =
    view === 'events' && !!date && !z.iso.date().safeParse(date).success;
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationDocument,
    pause: searchPending || invalidDate,
    variables: (offset): FederationQueryVariables => ({
      federation,
      events: view === 'events',
      people: view === 'people',
      competitors: view === 'competitors',
      search,
      onDate: view === 'events' && date ? date : null,
      competitorTypes: filters.type,
      ageGroups: filters.ageGroup,
      classes: filters.class,
      disciplines: filters.discipline,
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
      competedSince: filters.competedThisYear
        ? `${new Date().getFullYear()}-01-01`
        : null,
      offset,
    }),
    limit: 50,
    getTotal: (data) => data.federation?.[view]?.totalCount,
  });
  const item = data?.federation;
  const { total, page } = pagination;
  const isPending = fetching || searchPending;
  const availableDisciplines = new Set([
    ...(item?.competitorFacetsList ?? [])
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
    <>
      <TabMenu
        selected={filters.view}
        onSelect={(view) => {
          const selected = views.find((item) => item === view);
          if (selected)
            void setFilters(
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
        {views.map((view) => (
          <Tab
            key={view}
            id={view}
            title={
              { events: 'Events', people: 'People', competitors: 'Competitors' }[view]
            }
          >
            {null}
          </Tab>
        ))}
      </TabMenu>
      <div className="flex flex-wrap items-end gap-3 mb-2">
        <TextField
          name="search"
          label={view === 'events' ? 'Name, location or event ID' : 'Name or ID'}
          value={filters.search}
          onChange={(event) => setFilters({ search: event.target.value, page: null })}
        />
        {view === 'events' && (
          <TextField
            name="date"
            label="Date"
            type="date"
            value={filters.date}
            onChange={(event) => setFilters({ date: event.target.value, page: null })}
          />
        )}
        {view === 'competitors' && (
          <>
            {facets.map(({ key, label }) => {
              const counts = new Map(
                (item?.competitorFacetsList ?? [])
                  .filter((option) => option.facet === key)
                  .map((option) => [
                    key === 'type' ? option.value!.toUpperCase() : option.value!,
                    option.count ?? 0,
                  ]),
              );
              const values = [...new Set([...counts.keys(), ...filters[key]])].toSorted(
                (a, b) => a.localeCompare(b, 'en', { numeric: true }),
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
                        : key === 'class' && federation === 'csts'
                          ? (formatCstsClass(value) ?? value)
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
        )}
      </div>
      <FormError error={invalidDate ? 'Invalid date' : error} />
      <p role="status" className="my-2 text-sm text-neutral-11">
        {isPending
          ? 'Loading…'
          : !item?.[view] || invalidDate || error
            ? null
            : total === 0
              ? '0 results'
              : `${(page - 1) * 50 + 1}–${Math.min(page * 50, total)} of ${total}`}
      </p>
      {!invalidDate && !error && item && (
        <>
          {view === 'events' && item.events && (
            <Table
              columns={['date', 'event', 'status', 'location', 'country', 'competitions']}
              busy={isPending}
              empty="No events found"
              className="col-full-width"
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
          {view === 'competitors' && item.competitors && (
            <div className="col-full-width overflow-x-auto" aria-busy={isPending}>
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
                  const ageGroups = Object.keys(progressByAge).toSorted();
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
                              <td
                                rowSpan={ageGroups.length}
                                className="px-2 py-1 align-top"
                              >
                                {row.competitorType.toLowerCase()}
                              </td>
                            </>
                          )}
                          <td className="px-2 py-1 whitespace-nowrap align-top">
                            {ageGroup || '–'}
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
                                          {(federation === 'csts'
                                            ? formatCstsClass(p.category?.class)
                                            : null) ?? p.category?.class}
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
          {view === 'people' && item.people && (
            <Table
              columns={['person', 'age group']}
              busy={isPending}
              empty="No people found"
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
                  <td>{row.ageGroup || '-'}</td>
                </tr>
              ))}
            </Table>
          )}
          {item[view] && <Pagination {...pagination} />}
        </>
      )}
    </>
  );
}
