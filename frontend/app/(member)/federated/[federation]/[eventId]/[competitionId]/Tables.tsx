import { Table } from '@/ui/Table';
import Link from 'next/link';
import { LocalPersonLinks } from '@/ui/federation-links';
import { formatCoupleName, formatResultStatus } from '@/ui/format';
import type {
  CompetitionParticipantFragment,
  FederationCompetitionQuery,
} from '@/graphql/Federation';

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
          'local matches',
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
              <td>
                <LocalMatches participant={result.competitor} />
              </td>
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
        {entries.map(({ competitor, competitorId, cancelled }) => {
          return (
            <tr key={competitorId} className="border-t border-neutral-4 align-top">
              <th scope="row">
                {competitor ? (
                  <Link
                    className="text-accent-11 underline"
                    href={`/federated/${federation}/competitors/${competitor?.externalId}`}
                    prefetch={false}
                  >
                    {competitor.name || competitor.id}
                  </Link>
                ) : (
                  competitorId
                )}
              </th>
              <td>{cancelled ? 'cancelled' : 'registered'}</td>
              <td>
                <LocalMatches participant={competitor} />
              </td>
            </tr>
          );
        })}
      </Table>
    </>
  );
}

function LocalMatches({
  participant,
}: {
  participant: CompetitionParticipantFragment | null;
}) {
  if (!participant) return null;

  return (
    <>
      <LocalPersonLinks people={participant.peopleList} />
      {participant.couplesList.map((couple) => (
        <div key={couple.id}>
          <Link className="text-accent-11 underline" href={`/pary/${couple.id}`}>
            {formatCoupleName(couple)}
          </Link>
        </div>
      ))}
    </>
  );
}
