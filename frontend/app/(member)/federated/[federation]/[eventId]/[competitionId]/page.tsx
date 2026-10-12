import Link from 'next/link';
import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import {
  FederationEventDocument,
  FederationCompetitionDocument,
} from '@/graphql/Federation';
import { CrawlerSourcesDocument } from '@/graphql/Crawler';

export const metadata = {
  title: 'Federated competition',
  robots: { index: false, follow: false },
};

export default async function CompetitionPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string; eventId: string; competitionId: string }>;
  searchParams: Promise<{ roundsPage?: string; page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId, competitionId } = await params;
  if (!/^[0-9]{1,18}$/.test(eventId) || !/^[0-9]{1,18}$/.test(competitionId)) notFound();
  const { roundsPage, page } = await searchParams;
  const currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  const currentRoundsPage = Math.max(1, Number.parseInt(roundsPage ?? '1', 10) || 1);
  const [
    { federationEventByFederationAndId: event },
    { competition, federationCompetitors: participants },
  ] = await Promise.all([
    executeGraphql(FederationEventDocument, { federation, eventId }),
    executeGraphql(FederationCompetitionDocument, {
      id: competitionId,
      participantsOffset: Math.min((currentPage - 1) * 20, 2_147_483_647),
      roundsOffset: Math.min((currentRoundsPage - 1) * 100, 2_147_483_647),
    }),
  ]);
  if (!event || !competition || competition.eventId !== eventId) notFound();
  const rounds = competition.rounds;
  if (currentRoundsPage > Math.max(1, Math.ceil(rounds.totalCount / 100))) notFound();
  if (!participants || currentPage > Math.max(1, Math.ceil(participants.totalCount / 20)))
    notFound();
  const crawler = auth.isSystemAdmin
    ? await executeGraphql(CrawlerSourcesDocument, { condition: { competitionId } })
    : null;
  const sources = crawler?.crawlerTargets?.nodes ?? [];
  const path = `/federated/${federation}/${eventId}/${competitionId}`;
  return (
    <Layout requireUser>
      <PageHeader
        title={competition.category?.name ?? competition.externalId}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: event.name || event.externalId,
            href: `/federated/${federation}/${eventId}`,
          },
          { label: competition.category?.name ?? competition.externalId },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>competition ID</dt>
        <dd>{competition.externalId}</dd>
        {federation === 'csts' && (
          <>
            <dt>source</dt>
            <dd>
              <CstsResultsLink
                eventId={event.externalId}
                competitionId={competition.externalId}
              >
                ČSTS results
              </CstsResultsLink>
            </dd>
          </>
        )}
        <dt>status</dt>
        <dd>{competition.status.toLowerCase().replaceAll('_', ' ')}</dd>
        <dt>type</dt>
        <dd>{competition.competitionType?.toLowerCase() || '-'}</dd>
        <dt>date</dt>
        <dd>{numericDateWithYearFormatter.format(new Date(competition.startDate))}</dd>
        <dt>check-in end</dt>
        <dd>{competition.checkInEnd || '-'}</dd>
        <dt>participants reported</dt>
        <dd>{competition.participantsTotal ?? '-'}</dd>
        <dt>completed</dt>
        <dd>
          {competition.completedAt
            ? numericFullFormatter.format(new Date(competition.completedAt))
            : '-'}
        </dd>
      </dl>
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
        href={(page) => `${path}?page=${page}&roundsPage=${currentRoundsPage}`}
      />
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
        page={currentRoundsPage}
        href={(roundsPage) => `${path}?page=${currentPage}&roundsPage=${roundsPage}`}
      />
      {auth.isSystemAdmin && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Crawler</h2>
          {sources.map((source) => (
            <p key={source.id}>
              <Link
                className="text-accent-11 underline"
                href={`/admin/crawler/${source.id}`}
                prefetch={false}
              >
                {federation}:{source.kind}:{source.key}
              </Link>{' '}
              — fetch: {source.fetchStatus.toLowerCase()}, process:{' '}
              {source.processStatus.toLowerCase()}
            </p>
          ))}
          {sources.length === 0 && <p>No matching frontiers</p>}
        </>
      )}
    </Layout>
  );
}
