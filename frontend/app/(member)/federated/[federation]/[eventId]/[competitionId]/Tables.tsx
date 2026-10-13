import { Table } from '@/ui/Table';
import Link from 'next/link';
import { LocalPersonLinks } from '@/ui/federation-links';
import { formatCoupleName, formatResultStatus, scoringMethodLabels } from '@/ui/format';
import type { FederationCompetitionQuery } from '@/graphql/Federation';

type Competition = NonNullable<FederationCompetitionQuery['competition']>;

export function Results({
  federation,
  results,
}: {
  federation: string;
  results: Competition['results'];
}) {
  if (results.length === 0) return null;
  const showPoints = results.some((result) => result.pointGain != null);
  const showFinalGain = results.some((result) =>
    federation === 'csts' ? result.isFinal : result.finalGain != null,
  );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Results</h2>
      <Table
        columns={[
          'rank',
          'start number',
          'competitor',
          showPoints && 'points',
          showFinalGain && 'final gain',
        ]}
      >
        {results.map((result) => {
          const status = formatResultStatus(result.completionStatus);
          return (
            <tr key={result.competitorId} className="border-t border-neutral-4 align-top">
              <td className="whitespace-nowrap">
                {result.ranking}
                {result.rankingTo && result.rankingTo !== result.ranking
                  ? `–${result.rankingTo}`
                  : ''}
              </td>
              <td>{result.startNumber}</td>
              <th scope="row">
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
                {status && <div className="text-xs text-neutral-11">{status}</div>}
              </th>
              {showPoints && (
                <td>{result.pointGain == null ? null : Number(result.pointGain)}</td>
              )}
              {showFinalGain && (
                <td>
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
      </Table>
    </>
  );
}

export function Registrations({
  federation,
  entries,
  hasResults,
}: {
  federation: string;
  entries: Competition['entries'];
  hasResults: boolean;
}) {
  if (hasResults && entries.length === 0) return null;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">
        {hasResults ? 'Registrations without results' : 'Registrations'}
      </h2>
      <Table
        columns={['competitor', 'registration', 'local matches']}
        empty="No registrations"
      >
        {entries.map((entry) => {
          const participant = entry.competitor;
          const competitorPath = `/federated/${federation}/competitors/${participant?.externalId}`;
          return (
            <tr key={entry.competitorId} className="border-t border-neutral-4 align-top">
              <th scope="row">
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
              <td>{entry.cancelled ? 'cancelled' : 'registered'}</td>
              <td>
                {participant?.components.nodes.map(({ personId, person }) => {
                  if (
                    !person ||
                    (person.people.totalCount === 0 &&
                      participant.couplesList.length === 0)
                  ) {
                    return null;
                  }
                  const name =
                    person.canonicalName ||
                    [person.firstName, person.lastName].filter(Boolean).join(' ') ||
                    personId;
                  return (
                    <div key={personId}>
                      <LocalPersonLinks
                        federation={federation}
                        externalId={person.externalId}
                        people={person.people}
                      />
                      {person.people.totalCount === 0 && (
                        <span className="text-neutral-11">{name} (no person match)</span>
                      )}
                    </div>
                  );
                })}
                {participant &&
                  participant.components.totalCount >
                    participant.components.nodes.length && (
                    <div>
                      <Link className="text-accent-11 underline" href={competitorPath}>
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
      </Table>
    </>
  );
}

export function Rounds({
  rounds,
  href,
}: {
  rounds: Competition['rounds'];
  href: string;
}) {
  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Rounds</h2>
      <Table columns={['round', 'scoring method', 'results']} empty="No rounds">
        {rounds.map((round) => (
          <tr key={round.id} className="border-t border-neutral-4">
            <th scope="row">
              <Link
                href={`${href}/rounds/${encodeURIComponent(round.roundKey)}`}
                className="text-accent-11 underline"
                prefetch={false}
              >
                {round.roundLabel || round.roundKey}
              </Link>
            </th>
            <td>{scoringMethodLabels[round.scoringMethod]}</td>
            <td>{round.results.totalCount}</td>
          </tr>
        ))}
      </Table>
    </>
  );
}
