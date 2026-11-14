'use client';

import { formatCategoryName } from '@/ui/csts';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import {
  FederationEventCompetitionsDocument,
  FederationEventCompetitorsDocument,
} from '@/graphql/Federation';
import { formatCount, formatDateOnly } from '@/ui/format';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { compareNaturalText, compareText } from '@/lib/compare';
import { useState } from 'react';
import { ListHeader } from '@/ui/ListHeader';

type EventTablesProps = { federation: string; eventId: string };

export function EventTables(props: EventTablesProps) {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString
      .withDefault('competitions')
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );

  return (
    <TabMenu className="mt-4" selected={tab} onSelect={setTab}>
      <Tab id="competitions" title="Soutěže">
        <Competitions {...props} />
      </Tab>
      <Tab id="competitors" title="Soutěžící">
        <Competitors {...props} />
      </Tab>
    </TabMenu>
  );
}

function Competitors({ federation, eventId }: EventTablesProps) {
  const [search, setSearch] = useState('');
  const [{ data, fetching, error }] = useQuery({
    query: FederationEventCompetitorsDocument,
    variables: { federation, eventId },
  });
  const participations =
    data?.federationEventByFederationAndExternalId?.participationsList ?? [];
  const searchTerms = search
    .trim()
    .toLocaleLowerCase('cs')
    .normalize('NFD')
    .replaceAll(/\p{M}/gu, '')
    .split(/\s+/);
  const competitors = [...Map.groupBy(participations, (p) => p.competitorId)]
    .map(([competitorId, entries]) => ({
      competitorId,
      competitor: entries[0]!.competitor,
      entries,
      startNumbers: [...new Set(entries.flatMap((p) => p.startNumber || []))].toSorted(
        compareNaturalText,
      ),
    }))
    .filter(({ competitor, startNumbers }) => {
      const text = [competitor.name, ...startNumbers]
        .join(' ')
        .toLocaleLowerCase('cs')
        .normalize('NFD')
        .replaceAll(/\p{M}/gu, '');
      return searchTerms.every((term) => text.includes(term));
    })
    .toSorted((a, b) =>
      compareText(
        a.competitor.name || a.competitorId,
        b.competitor.name || b.competitorId,
      ),
    );
  const showStartNumber = participations.some((p) => p.startNumber);
  const showDate = participations.some(
    (p) => p.competition.startDate !== participations[0]?.competition.startDate,
  );

  return (
    <>
      <ListHeader
        search={search}
        onSearchChange={setSearch}
        searchPlaceholder="Jméno nebo startovní číslo"
        pagination={{ total: competitors.length, page: 1, limit: competitors.length }}
      />
      <Table
        busy={fetching}
        error={error}
        columns={[showStartNumber && 'Start. č.', 'Soutěžící', 'Soutěže']}
      >
        {competitors.map(({ competitorId, competitor, entries, startNumbers }) => (
          <tr key={competitorId} className="align-top">
            {showStartNumber && <td>{startNumbers.join(', ')}</td>}
            <th scope="row">
              <Link
                className={cn(
                  'text-accent-11',
                  entries.every((entry) => entry.cancelled) ? 'line-through' : 'underline',
                )}
                href={`/federated/${federation}/competitors/${competitor.externalId}`}
                prefetch={false}
              >
                {competitor.name || competitorId}
              </Link>
            </th>
            <td>
              <details>
                <summary className="cursor-pointer text-accent-11">
                  {formatCount(entries.length, {
                    one: 'soutěž',
                    few: 'soutěže',
                    other: 'soutěží',
                  })}
                </summary>
                <ul className="mt-1 space-y-1">
                  {entries.map(
                    ({ competition, competitionId, cancelled, startNumber }) => (
                      <li key={competitionId}>
                        <Link
                          className={cn(
                            'text-accent-11 underline',
                            (cancelled || competition.status === 'CANCELLED') &&
                              'line-through',
                          )}
                          href={`/federated/${federation}/${eventId}/${competition.externalId}`}
                          prefetch={false}
                        >
                          {formatCategoryName(competition) || competition.externalId}
                        </Link>
                        {startNumbers.length > 1 && startNumber && (
                          <span className="ml-2 text-xs text-neutral-11">
                            č. {startNumber}
                          </span>
                        )}
                        {showDate && (
                          <span className="ml-2 text-xs text-neutral-11">
                            {formatDateOnly(competition.startDate)}
                          </span>
                        )}
                      </li>
                    ),
                  )}
                </ul>
              </details>
            </td>
          </tr>
        ))}
      </Table>
    </>
  );
}

function Competitions({ federation, eventId }: EventTablesProps) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationEventCompetitionsDocument,
    variables: (offset) => ({
      federation,
      eventId,
      competitionsOffset: offset,
    }),
    limit: 100,
    getTotal: (data) =>
      data.federationEventByFederationAndExternalId?.competitions.totalCount,
  });
  const competitions =
    data?.federationEventByFederationAndExternalId?.competitions.nodes ?? [];
  const showDate = competitions.some(
    (comp) => comp.startDate !== competitions[0]?.startDate,
  );

  return (
    <>
      <Table
        busy={fetching}
        error={error}
        columns={[
          'Kategorie',
          showDate && 'Datum',
          'Prezence',
          <span key="participants" className="block text-right">
            Účastníci
          </span>,
        ]}
      >
        {competitions.map((comp) => (
          <tr key={comp.id}>
            <th scope="row">
              <Link
                className={cn(
                  'text-accent-11',
                  comp.status === 'CANCELLED' ? 'line-through' : 'underline',
                )}
                href={`/federated/${federation}/${eventId}/${comp.externalId}`}
                prefetch={false}
              >
                {formatCategoryName(comp) || comp.externalId}
              </Link>
            </th>
            {showDate && (
              <td className="whitespace-nowrap">{formatDateOnly(comp.startDate)}</td>
            )}
            <td>{comp.checkInEnd?.slice(0, 5)}</td>
            <td className="text-right">
              {comp.results.totalCount || comp.entries.totalCount}
            </td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
