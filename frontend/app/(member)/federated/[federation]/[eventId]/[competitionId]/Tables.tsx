'use client';

import Link from 'next/link';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import {
  CompetitionParticipantsDocument,
  CompetitionRoundsDocument,
} from '@/graphql/Federation';

export function Participants({ federation, id }: { federation: string; id: string }) {
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: CompetitionParticipantsDocument,
    variables: {
      id,
      participantsOffset: Math.min(Math.max(0, currentPage - 1) * 20, 2_147_483_647),
    },
  });
  const participants = data?.federationCompetitors;
  const total = participants?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 20)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);

  if (error) return <FormError error={error} />;
  if (!participants) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Registrations and results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'competitor',
                'registration',
                'rank',
                'start number',
                'points',
                'final gain',
                'status',
                'local matches',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {participants.nodes.map((participant) => {
              const entry = participant.competitionEntries.nodes[0];
              const result = participant.competitionResults.nodes[0];
              const competitorPath = `/federated/${federation}/competitors/${participant.externalId}`;
              return (
                <tr key={participant.id} className="border-t border-neutral-4 align-top">
                  <th scope="row" className="px-2 py-1 font-normal">
                    <Link
                      className="text-accent-11 underline"
                      href={competitorPath}
                      prefetch={false}
                    >
                      {participant.name || participant.id}
                    </Link>
                    <div className="text-xs">{participant.id}</div>
                  </th>
                  <td className="px-2 py-1">
                    {!entry ? '-' : entry.cancelled ? 'cancelled' : 'registered'}
                  </td>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {result?.ranking}
                    {result?.rankingTo && result?.rankingTo !== result?.ranking
                      ? `–${result?.rankingTo}`
                      : ''}
                  </td>
                  <td className="px-2 py-1">{result?.startNumber}</td>
                  <td className="px-2 py-1">{result?.pointGain}</td>
                  <td className="px-2 py-1">{result?.finalGain}</td>
                  <td className="px-2 py-1">{result?.completionStatus}</td>
                  <td className="px-2 py-1">
                    {participant.components.nodes.map(({ personId, person }) => (
                      <div key={personId}>
                        {person ? (
                          <>
                            <Link
                              className="font-mono text-xs text-accent-11 underline"
                              href={`/federated/${federation}/people/${person.externalId}`}
                              prefetch={false}
                            >
                              {personId}
                            </Link>
                            {' → '}
                            {person.people.nodes.length === 0
                              ? 'No local match'
                              : person.people.nodes.map((match, index) => (
                                  <span key={match.id}>
                                    {index > 0 && ', '}
                                    <Link
                                      className="text-accent-11 underline"
                                      href={`/clenove/${match.id}`}
                                    >
                                      {match.name}
                                    </Link>
                                  </span>
                                ))}
                            {person.people.totalCount > person.people.nodes.length && (
                              <Link
                                className="ml-1 text-accent-11 underline"
                                href={`/federated/${federation}/people/${person.externalId}`}
                              >
                                All {person.people.totalCount} matches
                              </Link>
                            )}
                          </>
                        ) : (
                          personId
                        )}
                      </div>
                    ))}
                    {participant.components.totalCount === 0 && 'No competitor members'}
                    {participant.components.totalCount >
                      participant.components.nodes.length && (
                      <div>
                        <Link className="text-accent-11 underline" href={competitorPath}>
                          All {participant.components.totalCount} members
                        </Link>
                      </div>
                    )}
                    {participant.couplesList.map(({ id }) => (
                      <div key={id}>
                        <Link className="text-accent-11 underline" href={`/pary/${id}`}>
                          Couple {id}
                        </Link>
                      </div>
                    ))}
                  </td>
                </tr>
              );
            })}
            {participants.nodes.length === 0 && (
              <tr>
                <td colSpan={8} className="px-2 py-1">
                  No registrations or results
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={participants.totalCount}
        limit={20}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}

export function Rounds({ id }: { id: string }) {
  const [currentPage, setPage] = useQueryState(
    'roundsPage',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: CompetitionRoundsDocument,
    variables: {
      id,
      roundsOffset: Math.min(Math.max(0, currentPage - 1) * 100, 2_147_483_647),
    },
  });
  const rounds = data?.competition?.rounds;
  const total = rounds?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 100)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);

  if (error) return <FormError error={error} />;
  if (!rounds) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Rounds</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['round', 'key', 'scoring method', 'results'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rounds.nodes.map((round) => (
              <tr key={round.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {round.roundLabel || round.roundKey}
                </th>
                <td className="px-2 py-1">{round.roundKey}</td>
                <td className="px-2 py-1">{round.scoringMethod.toLowerCase()}</td>
                <td className="px-2 py-1">{round.results.totalCount}</td>
              </tr>
            ))}
            {rounds.nodes.length === 0 && (
              <tr>
                <td colSpan={4} className="px-2 py-1">
                  No rounds
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={rounds.totalCount}
        limit={100}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}
