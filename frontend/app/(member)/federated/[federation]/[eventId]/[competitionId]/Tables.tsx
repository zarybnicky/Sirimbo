'use client';

import Link from 'next/link';
import { useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { formatCoupleName, formatResultStatus, scoringMethodLabels } from '@/ui/format';
import {
  CompetitionResultsDocument,
  CompetitionRegistrationsDocument,
  CompetitionRoundsDocument,
} from '@/graphql/Federation';

export function Results({ federation, id }: { federation: string; id: string }) {
  const [{ data, error }] = useQuery({
    query: CompetitionResultsDocument,
    variables: { id },
  });
  const results = data?.competition?.results;

  if (error) return <FormError error={error} />;
  if (!results) return <Spinner />;
  const showPoints = results.some((result) => result.pointGain != null);
  const showFinalGain = results.some((result) =>
    federation === 'csts' ? result.isFinal : result.finalGain != null,
  );

  return (
    <>
      {results.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Results</h2>
          <div className="col-full-width overflow-x-auto">
            <table className="w-full text-left text-sm tabular-nums">
              <thead>
                <tr>
                  {['rank', 'start number', 'competitor'].map((label) => (
                    <th key={label} scope="col" className="px-2 py-1">
                      {label}
                    </th>
                  ))}
                  {showPoints && (
                    <th scope="col" className="px-2 py-1">
                      points
                    </th>
                  )}
                  {showFinalGain && (
                    <th scope="col" className="px-2 py-1">
                      final gain
                    </th>
                  )}
                </tr>
              </thead>
              <tbody>
                {results.map((result) => {
                  const status = formatResultStatus(result.completionStatus);
                  return (
                    <tr
                      key={result.competitorId}
                      className="border-t border-neutral-4 align-top"
                    >
                      <td className="px-2 py-1 whitespace-nowrap">
                        {result.ranking}
                        {result.rankingTo && result.rankingTo !== result.ranking
                          ? `–${result.rankingTo}`
                          : ''}
                      </td>
                      <td className="px-2 py-1">{result.startNumber}</td>
                      <th scope="row" className="px-2 py-1 font-normal">
                        {result.competitor ? (
                          <Link
                            className="text-accent-11 underline"
                            href={`/federated/${federation}/competitors/${result.competitor.externalId}`}
                            prefetch={false}
                          >
                            {result.competitor.name || result.competitorId}
                          </Link>
                        ) : (
                          result.competitorId
                        )}
                        {status && (
                          <div className="text-xs text-neutral-11">{status}</div>
                        )}
                      </th>
                      {showPoints && (
                        <td className="px-2 py-1">
                          {result.pointGain == null ? null : Number(result.pointGain)}
                        </td>
                      )}
                      {showFinalGain && (
                        <td className="px-2 py-1">
                          {federation === 'csts'
                            ? result.isFinal
                              ? 'F'
                              : ''
                            : result.finalGain == null
                              ? null
                              : Number(result.finalGain)}
                        </td>
                      )}
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </>
      )}
      <Registrations federation={federation} id={id} hasResults={results.length > 0} />
    </>
  );
}

function Registrations({
  federation,
  id,
  hasResults,
}: {
  federation: string;
  id: string;
  hasResults: boolean;
}) {
  const [{ data, error }] = useQuery({
    query: CompetitionRegistrationsDocument,
    variables: { id },
  });
  const entries = data?.competition?.entries;

  if (error) return <FormError error={error} />;
  if (!entries) return <Spinner />;

  if (hasResults && entries.length === 0) return null;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">
        {hasResults ? 'Registrations without results' : 'Registrations'}
      </h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['competitor', 'registration', 'local matches'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {entries.map((entry) => {
              const participant = entry.competitor;
              const competitorPath = `/federated/${federation}/competitors/${participant?.externalId}`;
              return (
                <tr
                  key={entry.competitorId}
                  className="border-t border-neutral-4 align-top"
                >
                  <th scope="row" className="px-2 py-1 font-normal">
                    {participant ? (
                      <Link
                        className="text-accent-11 underline"
                        href={competitorPath}
                        prefetch={false}
                      >
                        {participant.name || participant.id}
                      </Link>
                    ) : (
                      entry.competitorId
                    )}
                  </th>
                  <td className="px-2 py-1">
                    {entry.cancelled ? 'cancelled' : 'registered'}
                  </td>
                  <td className="px-2 py-1">
                    {participant?.components.nodes.map(
                      ({ personId, person }) =>
                        person &&
                        person.people.nodes.length > 0 && (
                          <div key={personId}>
                            <Link
                              className="text-accent-11 underline"
                              href={`/federated/${federation}/people/${person.externalId}`}
                              prefetch={false}
                            >
                              {person.canonicalName ||
                                [person.firstName, person.lastName]
                                  .filter(Boolean)
                                  .join(' ') ||
                                personId}
                            </Link>
                            {' → '}
                            {person.people.nodes.map((match, index) => (
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
                          </div>
                        ),
                    )}
                    {participant &&
                      participant.components.totalCount >
                        participant.components.nodes.length && (
                        <div>
                          <Link
                            className="text-accent-11 underline"
                            href={competitorPath}
                          >
                            All {participant.components.totalCount} members
                          </Link>
                        </div>
                      )}
                    {participant?.couplesList.map((couple) => (
                      <div key={couple.id}>
                        <Link
                          className="text-accent-11 underline"
                          href={`/pary/${couple.id}`}
                        >
                          {formatCoupleName(couple)}
                        </Link>
                      </div>
                    ))}
                  </td>
                </tr>
              );
            })}
            {entries.length === 0 && (
              <tr>
                <td colSpan={3} className="px-2 py-1">
                  No registrations
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </>
  );
}

export function Rounds({ id, href }: { id: string; href: string }) {
  const [{ data, error }] = useQuery({
    query: CompetitionRoundsDocument,
    variables: { id },
  });
  const rounds = data?.competition?.rounds;

  if (error) return <FormError error={error} />;
  if (!rounds) return <Spinner />;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Rounds</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['round', 'scoring method', 'results'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rounds.map((round) => (
              <tr key={round.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    href={`${href}/rounds/${encodeURIComponent(round.roundKey)}`}
                    className="text-accent-11 underline"
                    prefetch={false}
                  >
                    {round.roundLabel || round.roundKey}
                  </Link>
                </th>
                <td className="px-2 py-1">{scoringMethodLabels[round.scoringMethod]}</td>
                <td className="px-2 py-1">{round.results.totalCount}</td>
              </tr>
            ))}
            {rounds.length === 0 && (
              <tr>
                <td colSpan={3} className="px-2 py-1">
                  No rounds
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
    </>
  );
}
