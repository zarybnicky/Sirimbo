'use client';

import Link from 'next/link';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { FederationPersonOfficialAssignmentsDocument } from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { FormError } from '@/ui/form';
import { formatDateOnly } from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { Spinner } from '@/ui/Spinner';

export function OfficialAssignments({
  federation,
  id,
}: {
  federation: string;
  id: string;
}) {
  const [currentPage, setPage] = useQueryState(
    'officialsPage',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: FederationPersonOfficialAssignmentsDocument,
    variables: {
      id,
      offset: Math.min(Math.max(0, currentPage - 1) * 20, 2_147_483_647),
    },
  });
  const events = data?.federationEvents;
  const total = events?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 20)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
  const base = `/federated/${federation}`;
  if (error) return <FormError error={error} />;
  if (!events) return <Spinner />;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Official assignments</h2>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['date', 'event', 'assignments'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {events.nodes.map((event) => {
              const roles = [
                ...new Set([
                  ...event.officials.nodes.map((official) => official.role),
                  ...event.competitionsList.flatMap((competition) =>
                    competition.officials.nodes.map((official) => official.role),
                  ),
                ]),
              ].toSorted();
              return (
                <tr key={event.id} className="border-t border-neutral-4 align-top">
                  <td className="px-2 py-1 whitespace-nowrap">
                    {formatDateOnly(event.startDate)}
                  </td>
                  <th scope="row" className="px-2 py-1 font-normal">
                    <Link
                      href={`${base}/${event.externalId}`}
                      prefetch={false}
                      className={cn(
                        'text-accent-11',
                        event.status === 'CANCELLED' ? 'line-through' : 'underline',
                      )}
                    >
                      {event.name || event.externalId}
                    </Link>
                  </th>
                  <td className="px-2 py-1 space-y-2">
                    {roles.map((role) => {
                      const competitions = event.competitionsList.filter((competition) =>
                        competition.officials.nodes.some(
                          (official) => official.role === role,
                        ),
                      );
                      return (
                        <div key={role}>
                          {role.toLowerCase().replaceAll('_', ' ')}
                          {event.officials.nodes
                            .filter((official) => official.role === role)
                            .map((official) => (
                              <span key={official.discipline} className="text-neutral-11">
                                {[official.discipline, official.grade]
                                  .filter(Boolean)
                                  .map((value) => ` · ${value}`)
                                  .join('')}
                              </span>
                            ))}
                          {competitions.length > 0 && (
                            <details>
                              <summary className="cursor-pointer text-neutral-11">
                                {competitions.length}{' '}
                                {competitions.length === 1
                                  ? 'competition'
                                  : 'competitions'}
                              </summary>
                              <ul className="space-y-1 mt-1">
                                {competitions.map((competition) => (
                                  <li key={competition.id}>
                                    <Link
                                      href={`${base}/${event.externalId}/${competition.externalId}`}
                                      prefetch={false}
                                      className={cn(
                                        'text-accent-11',
                                        competition.status === 'CANCELLED'
                                          ? 'line-through'
                                          : 'underline',
                                      )}
                                    >
                                      {competition.category?.name ||
                                        competition.externalId}
                                    </Link>
                                    {competition.startDate !== event.startDate &&
                                      competition.startDate && (
                                        <span className="text-neutral-11">
                                          {' '}
                                          · {formatDateOnly(competition.startDate)}
                                        </span>
                                      )}
                                  </li>
                                ))}
                              </ul>
                            </details>
                          )}
                        </div>
                      );
                    })}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
      <Pagination
        total={events.totalCount}
        limit={20}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}
