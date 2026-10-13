'use client';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
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
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationPersonOfficialAssignmentsDocument,
    variables: (offset) => ({
      id,
      offset,
    }),
    limit: 20,
    pageKey: 'officialsPage',
    getTotal: (data) => data?.federationEvents?.totalCount,
  });
  const events = data?.federationEvents;
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
      <Table columns={['date', 'event', 'assignments']}>
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
              <td className="whitespace-nowrap">{formatDateOnly(event.startDate)}</td>
              <th scope="row">
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
              <td className="space-y-2">
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
                            {competitions.length === 1 ? 'competition' : 'competitions'}
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
                                  {competition.category?.name || competition.externalId}
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
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
