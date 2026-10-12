import Link from 'next/link';
import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Results, Rounds } from './Tables';
import { formatDateOnly, dateTimeFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import {
  FederationEventDocument,
  FederationCompetitionDocument,
} from '@/graphql/Federation';
import { CrawlerSourcesDocument } from '@/graphql/Crawler';

type Props = {
  params: Promise<{ federation: string; eventId: string; competitionId: string }>;
};

const getCompetition = cache(
  async (federation: string, eventId: string, competitionId: string) => {
    const [{ federationEventByFederationAndExternalId: event }, { competition }] =
      await Promise.all([
        executeGraphql(FederationEventDocument, { federation, eventId }),
        executeGraphql(FederationCompetitionDocument, {
          federation,
          competitionId,
        }),
      ]);
    if (!event || !competition || competition.eventId !== event.id) notFound();
    return { event, competition };
  },
);

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, eventId, competitionId } = await params;
  const { competition } = await getCompetition(federation, eventId, competitionId);
  return {
    title: `${competition.category?.name ?? competitionId} · ${formatDateOnly(competition.startDate)}`,
    robots: { index: false, follow: false },
  };
}

export default async function CompetitionPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId, competitionId } = await params;
  const { event, competition } = await getCompetition(federation, eventId, competitionId);
  const crawler = auth.isSystemAdmin
    ? await executeGraphql(CrawlerSourcesDocument, {
        condition: { competitionId: competition.id },
      })
    : null;
  const sources = crawler?.crawlerTargets?.nodes ?? [];
  return (
    <Layout requireUser>
      <PageHeader
        title={competition.category?.name ?? competition.externalId}
        subtitle={
          <span className="flex gap-2">
            <span className="shrink-0">{formatDateOnly(competition.startDate)} ·</span>
            <span className="truncate" title={event.name || eventId}>
              {event.name || eventId}
            </span>
          </span>
        }
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: event.name || event.externalId,
            href: `/federated/${federation}/${eventId}`,
            truncate: true,
          },
          { label: competition.category?.name ?? competition.externalId },
        ]}
      />
      <dl className="text-sm tabular">
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
        <dd>{competition.competitionType?.toLowerCase().replaceAll('_', ' ') || '-'}</dd>
        <dt>check-in end</dt>
        <dd>{competition.checkInEnd?.slice(0, 5) || '-'}</dd>
        <dt>participants reported</dt>
        <dd>{competition.participantsTotal ?? '-'}</dd>
        <dt>completed</dt>
        <dd>
          {competition.completedAt
            ? dateTimeFormatter.format(new Date(competition.completedAt))
            : '-'}
        </dd>
      </dl>
      <Results federation={federation} id={competition.id} />
      <Rounds
        id={competition.id}
        href={`/federated/${federation}/${eventId}/${competitionId}`}
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
