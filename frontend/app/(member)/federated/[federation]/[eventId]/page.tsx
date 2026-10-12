import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { formatDateOnly } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationEventDocument } from '@/graphql/Federation';
import { Competitions, Sources } from './Tables';

export const metadata = {
  title: 'Federated event',
  robots: { index: false, follow: false },
};

export default async function EventPage({
  params,
}: {
  params: Promise<{ federation: string; eventId: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId } = await params;
  const { federationEventByFederationAndExternalId: event } = await executeGraphql(
    FederationEventDocument,
    { federation, eventId },
  );
  if (!event) notFound();
  const title = event.name || event.externalId;
  return (
    <Layout requireUser>
      <PageHeader
        title={title}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          { label: title },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>event ID</dt>
        <dd>{event.externalId}</dd>
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
        <dt>date</dt>
        <dd>
          {formatDateOnly(event.startDate)}
          {event.endDate && event.endDate !== event.startDate && (
            <> – {formatDateOnly(event.endDate)}</>
          )}
        </dd>
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
      {auth.isSystemAdmin && <Sources federation={federation} eventId={event.id} />}
    </Layout>
  );
}
