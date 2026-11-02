import { Table } from '@/ui/Table';
import Link from 'next/link';
import { LocalPersonLinks } from '@/ui/federation-links';
import { formatPointsAndFinals, formatResultStatus } from '@/ui/format';
import type {
  CompetitionParticipantFragment,
  FederationCompetitionQuery,
} from '@/graphql/Federation';
import { cn } from '@/lib/cn';

type Competition = NonNullable<FederationCompetitionQuery['competition']>;

export function Results({
  federation,
  results,
}: {
  federation: string;
  results: Competition['results'];
}) {
  if (results.length === 0) return null;
  const showPoints = results.some((x) => !!x.pointGain);
  const showLocalMatches = results.some(
    (x) => !!x.competitor?.couplesList.length || !!x.competitor?.peopleList.length,
  );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Výsledky</h2>
      <Table
        columns={[
          'Umístění',
          'Start. č.',
          'Soutěžící',
          'Soutěží za',
          showPoints && 'Body',
          showLocalMatches && 'V klubu',
        ]}
      >
        {results.map((result) => (
          <tr key={result.competitorId} className="border-t border-neutral-4 align-top">
            <td className="whitespace-nowrap">
              {result.ranking}
              {result.rankingTo && result.rankingTo !== result.ranking
                ? `–${result.rankingTo}.`
                : '.'}
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
              <div className="text-xs text-neutral-11">
                {formatResultStatus(result.completionStatus)}
              </div>
            </th>
            <td>
              {[result.club?.name ?? result.represents, result.country]
                .filter(Boolean)
                .join(', ')}
            </td>
            {showPoints && (
              <td className="whitespace-nowrap">
                {formatPointsAndFinals(result, 'gain')}
              </td>
            )}
            {showLocalMatches && (
              <td>
                <LocalMatches participant={result.competitor} />
              </td>
            )}
          </tr>
        ))}
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

  const showLocalMatches = entries.some(
    (x) => !!x.competitor?.couplesList.length || !!x.competitor?.peopleList.length,
  );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">
        {hasResults ? 'No-show' : 'Přihlášky'}
      </h2>
      <Table columns={['Jméno', 'Soutěží za', showLocalMatches && 'V klubu']}>
        {entries.map(
          ({ competitor, competitorId, cancelled, club, represents, country }) => (
            <tr key={competitorId} className="border-t border-neutral-4 align-top">
              <th scope="row">
                {competitor ? (
                  <Link
                    className={cn(
                      'text-accent-11 underline',
                      cancelled ? 'line-through' : '',
                    )}
                    href={`/federated/${federation}/competitors/${competitor?.externalId}`}
                    prefetch={false}
                  >
                    {competitor.name || competitor.id}
                  </Link>
                ) : (
                  competitorId
                )}
              </th>
              <td>{[club?.name ?? represents, country].filter(Boolean).join(', ')}</td>
              {showLocalMatches && (
                <td>
                  <LocalMatches participant={competitor} />
                </td>
              )}
            </tr>
          ),
        )}
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
            {couple.name}
          </Link>
        </div>
      ))}
    </>
  );
}
