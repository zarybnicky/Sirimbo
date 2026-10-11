'use client';

import Link from 'next/link';
import { useTransition } from 'react';
import {
  debounce,
  parseAsInteger,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import { TextField } from '@/ui/fields/text';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter } from '@/ui/format';
import type {
  IBrowseEventsResult,
  IBrowsePeopleResult,
  IBrowseSearchCompetitorsResult,
} from '../federated.queries';

const views = ['events', 'people', 'competitors'] as const;

export type BrowserResult = { total: number; page: number } & (
  | { view: 'events'; rows: IBrowseEventsResult[] }
  | {
      view: 'people' | 'competitors';
      rows: (IBrowsePeopleResult | IBrowseSearchCompetitorsResult)[];
    }
);

export function Browser({
  federation,
  result,
}: {
  federation: string;
  result: BrowserResult;
}) {
  const [isPending, startTransition] = useTransition();
  const [filters, setFilters] = useQueryStates(
    {
      search: parseAsString.withDefault(''),
      date: parseAsString.withDefault(''),
      page: parseAsInteger,
      view: parseAsStringLiteral(views).withDefault('events'),
    },
    { shallow: false, scroll: false, startTransition },
  );

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
                  setFilters(
                    { search: event.target.value, page: null },
                    { limitUrlUpdates: debounce(300) },
                  )
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
            <p role="status" className="my-2 text-sm text-neutral-11">
              {isPending || result.view !== view
                ? 'Loading…'
                : result.total === 0
                  ? '0 results'
                  : `${(result.page - 1) * 50 + 1}–${Math.min(result.page * 50, result.total)} of ${result.total}`}
            </p>
            {result.view === view && (
              <div className="contents" aria-busy={isPending}>
                {result.view === 'events' ? (
                  <div className="col-full-width overflow-x-auto">
                    <table className="w-full text-left text-sm tabular-nums">
                      <thead>
                        <tr>
                          {[
                            'date',
                            'event',
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
                              {numericDateWithYearFormatter.format(event.start_date)}
                              {event.end_date &&
                                event.end_date.getTime() !==
                                  event.start_date.getTime() && (
                                  <>
                                    {' '}
                                    –{' '}
                                    {numericDateWithYearFormatter.format(event.end_date)}
                                  </>
                                )}
                            </td>
                            <th scope="row" className="px-2 py-1 font-normal">
                              <Link
                                className="text-accent-11 underline"
                                href={`/admin/federated/${federation}/${event.id}`}
                                prefetch={false}
                              >
                                {event.name || event.external_id}
                              </Link>
                            </th>
                            <td className="px-2 py-1">{event.external_id}</td>
                            <td className="px-2 py-1">{event.location || event.city}</td>
                            <td className="px-2 py-1">{event.country}</td>
                            <td className="px-2 py-1">{event.competitions}</td>
                          </tr>
                        ))}
                        {result.rows.length === 0 && (
                          <tr>
                            <td colSpan={6} className="px-2 py-1">
                              No events found
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </table>
                  </div>
                ) : (
                  <div className="overflow-x-auto">
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
                                href={`/admin/federated/${federation}/${result.view}/${row.external_id}`}
                                prefetch={false}
                              >
                                {row.name || row.id}
                              </Link>
                            </th>
                            <td className="px-2 py-1">{row.external_id}</td>
                            <td className="px-2 py-1">{row.age_group || '-'}</td>
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
              </div>
            )}
          </Tab>
        ))}
      </TabMenu>
    </>
  );
}
