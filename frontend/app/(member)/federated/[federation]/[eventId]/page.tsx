import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { formatDateOnly } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationEventDocument } from '@/graphql/Federation';
import { Competitions, Sources } from './Tables';
import { Officials } from './Officials';

type Props = {
  params: Promise<{ federation: string; eventId: string }>;
};

const getEvent = cache(async (federation: string, eventId: string) => {
  const { federationEventByFederationAndExternalId: event } = await executeGraphql(
    FederationEventDocument,
    { federation, eventId },
  );
  if (!event) notFound();
  return {
    event,
    title: event.name || event.externalId,
    date:
      formatDateOnly(event.startDate) +
      (event.endDate && event.endDate !== event.startDate
        ? ` – ${formatDateOnly(event.endDate)}`
        : ''),
  };
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, eventId } = await params;
  const { title, date } = await getEvent(federation, eventId);
  return { title: `${title} · ${date}`, robots: { index: false, follow: false } };
}

export default async function EventPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId } = await params;
  const { event, title, date } = await getEvent(federation, eventId);
  return (
    <Layout requireUser>
      <PageHeader
        title={title}
        subtitle={date}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          { label: title, truncate: true },
        ]}
      />
      <dl className="text-sm tabular">
        {federation === 'csts' && (
          <>
            <dt>source</dt>
            <dd>
              <CstsResultsLink eventId={event.externalId}>ČSTS results</CstsResultsLink>
            </dd>
          </>
        )}
        <dt>status</dt>
        <dd>{event.status.toLowerCase().replaceAll('_', ' ')}</dd>
        <dt>venue</dt>
        <dd>
          {[
            event.location,
            event.streetAddress,
            event.city,
            event.postalCode,
            event.country,
          ]
            .filter(Boolean)
            .join(', ') || '-'}
        </dd>
      </dl>
      <Competitions federation={federation} eventId={eventId} />
      <Officials federation={federation} officials={event.officials.nodes} />
      {auth.isSystemAdmin && <Sources federation={federation} eventId={event.id} />}
    </Layout>
  );
}
