'use client';

import Link from 'next/link';
import { useEffect, useState } from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import {
  parseAsInteger,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { TextField } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly } from '@/ui/format';
import { FederationDocument, type FederationQuery } from '@/graphql/Federation';

type FederationData = NonNullable<FederationQuery['federation']>;

const views = ['events', 'people', 'competitors'] as const;

type BrowserResult = { total: number; page: number } & (
  | { view: 'events'; rows: NonNullable<FederationData['events']>['nodes'] }
  | {
      view: 'people' | 'competitors';
      rows: (
        | NonNullable<FederationData['people']>['nodes'][number]
        | NonNullable<FederationData['competitors']>['nodes'][number]
      )[];
    }
);

export function Browser({ federation }: { federation: string }) {
  const [filters, setFilters] = useQueryStates(
    {
      search: parseAsString.withDefault(''),
      date: parseAsString.withDefault(''),
      page: parseAsInteger,
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
  const page = Math.max(1, filters.page ?? 1);
  const searchPending = search !== filters.search.trim();
  const invalidDate =
    view === 'events' && !!date && !z.iso.date().safeParse(date).success;
  const [{ data, fetching, error }] = useQuery({
    query: FederationDocument,
    pause: searchPending || invalidDate,
    variables: {
      federation,
      events: view === 'events',
      people: view === 'people',
      competitors: view === 'competitors',
      eventCondition: { search, onDate: view === 'events' && date ? date : null },
      personCondition: { search },
      competitorCondition: { search },
      offset: Math.min((page - 1) * 50, 2_147_483_647),
    },
  });
  const item = data?.federation;
  const total = item?.[view]?.totalCount ?? 0;
  const lastPage = Math.max(1, Math.ceil(total / 50));
  const isPending = fetching || searchPending;

  useEffect(() => {
    if (item?.[view] && !isPending && !error && !invalidDate && filters.page != null) {
      const clamped = Math.max(1, Math.min(filters.page, lastPage));
      if (filters.page !== clamped) void setFilters({ page: clamped });
    }
  }, [item, view, isPending, error, invalidDate, filters.page, lastPage, setFilters]);

  const result: BrowserResult | null =
    !item?.[view] || invalidDate || error
      ? null
      : view === 'events'
        ? { view, total, page, rows: item.events?.nodes ?? [] }
        : { view, total, page, rows: item[view]?.nodes ?? [] };

  return (
    <>
      <TabMenu
        selected={filters.view}
        onSelect={(view) => {
          const selected = views.find((item) => item === view);
          if (selected)
            void setFilters(
              { view: selected, search: null, date: null, page: null },
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
            <div className="flex flex-wrap items-end gap-3 mb-2">
              <TextField
                name="search"
                label={view === 'events' ? 'Name, location or event ID' : 'Name or ID'}
                value={filters.search}
                onChange={(event) =>
                  setFilters({ search: event.target.value, page: null })
                }
              />
              {view === 'events' && (
                <TextField
                  name="date"
                  label="Date"
                  type="date"
                  value={filters.date}
                  onChange={(event) =>
                    setFilters({ date: event.target.value, page: null })
                  }
                />
              )}
            </div>
            <FormError error={invalidDate ? 'Invalid date' : error} />
            <p role="status" className="my-2 text-sm text-neutral-11">
              {isPending
                ? 'Loading…'
                : !result
                  ? null
                  : result.total === 0
                    ? '0 results'
                    : `${(result.page - 1) * 50 + 1}–${Math.min(result.page * 50, result.total)} of ${result.total}`}
            </p>
            {result && (
              <>
                {result.view === 'events' ? (
                  <div className="col-full-width overflow-x-auto" aria-busy={isPending}>
                    <table className="w-full text-left text-sm tabular-nums">
                      <thead>
                        <tr>
                          {[
                            'date',
                            'event',
                            'status',
                            'event ID',
                            'location',
                            'country',
                            'competitions',
                          ].map((label) => (
                            <th key={label} scope="col" className="px-2 py-1">
                              {label}
                            </th>
                          ))}
                        </tr>
                      </thead>
                      <tbody>
                        {result.rows.map((event) => (
                          <tr key={event.id} className="border-t border-neutral-4">
                            <td className="px-2 py-1 whitespace-nowrap">
                              {formatDateOnly(event.startDate)}
                              {event.endDate && event.endDate !== event.startDate && (
                                <> – {formatDateOnly(event.endDate)}</>
                              )}
                            </td>
                            <th scope="row" className="px-2 py-1 font-normal">
                              <Link
                                className="text-accent-11 underline"
                                href={`/federated/${federation}/${event.externalId}`}
                                prefetch={false}
                              >
                                {event.name || event.externalId}
                              </Link>
                            </th>
                            <td className="px-2 py-1 whitespace-nowrap">
                              {event.status.toLowerCase().replaceAll('_', ' ')}
                            </td>
                            <td className="px-2 py-1">{event.externalId}</td>
                            <td className="px-2 py-1">{event.location || event.city}</td>
                            <td className="px-2 py-1">{event.country}</td>
                            <td className="px-2 py-1">{event.competitions.totalCount}</td>
                          </tr>
                        ))}
                        {result.rows.length === 0 && (
                          <tr>
                            <td colSpan={7} className="px-2 py-1">
                              No events found
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </table>
                  </div>
                ) : (
                  <div className="overflow-x-auto" aria-busy={isPending}>
                    <table className="w-full text-left text-sm">
                      <thead>
                        <tr>
                          {[
                            result.view === 'people' ? 'person' : 'competitor',
                            'ID',
                            'age group',
                          ].map((label) => (
                            <th key={label} scope="col" className="px-2 py-1">
                              {label}
                            </th>
                          ))}
                        </tr>
                      </thead>
                      <tbody>
                        {result.rows.map((row) => (
                          <tr key={row.id} className="border-t border-neutral-4">
                            <th scope="row" className="px-2 py-1 font-normal">
                              <Link
                                className="text-accent-11 underline"
                                href={`/federated/${federation}/${result.view}/${row.externalId}`}
                                prefetch={false}
                              >
                                {row.name ||
                                  ('firstName' in row &&
                                    [row.firstName, row.lastName]
                                      .filter(Boolean)
                                      .join(' ')) ||
                                  row.id}
                              </Link>
                            </th>
                            <td className="px-2 py-1">{row.externalId}</td>
                            <td className="px-2 py-1">{row.ageGroup || '-'}</td>
                          </tr>
                        ))}
                        {result.rows.length === 0 && (
                          <tr>
                            <td colSpan={3} className="px-2 py-1">
                              No {result.view} found
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </table>
                  </div>
                )}
                <Pagination
                  total={result.total}
                  limit={50}
                  page={result.page}
                  onPageChange={(page) => setFilters({ page }, { history: 'push' })}
                />
              </>
            )}
          </Tab>
        ))}
      </TabMenu>
    </>
  );
}
