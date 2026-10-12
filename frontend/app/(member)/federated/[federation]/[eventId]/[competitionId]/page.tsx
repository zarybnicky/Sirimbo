import Link from 'next/link';
import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Participants, Rounds } from './Tables';
import { formatDateOnly, numericFullFormatter } from '@/ui/format';
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
}: {
  params: Promise<{ federation: string; eventId: string; competitionId: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId, competitionId } = await params;
  if (!/^[0-9]{1,18}$/.test(eventId) || !/^[0-9]{1,18}$/.test(competitionId)) notFound();
  const [{ federationEventByFederationAndId: event }, { competition }] =
    await Promise.all([
      executeGraphql(FederationEventDocument, { federation, eventId }),
      executeGraphql(FederationCompetitionDocument, {
        id: competitionId,
      }),
    ]);
  if (!event || !competition || competition.eventId !== eventId) notFound();
  const crawler = auth.isSystemAdmin
    ? await executeGraphql(CrawlerSourcesDocument, { condition: { competitionId } })
    : null;
  const sources = crawler?.crawlerTargets?.nodes ?? [];
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
        <dd>{formatDateOnly(competition.startDate)}</dd>
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
      <Participants federation={federation} id={competitionId} />
      <Rounds id={competitionId} />
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
