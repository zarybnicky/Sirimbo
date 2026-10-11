import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationEventDocument } from '@/graphql/Federation';
import { runQuery } from '@/lib/server/postgresql';
import { browseSourceFrontiers } from '../../federated.queries';

export const metadata = {
  title: 'Federated event',
  robots: { index: false, follow: false },
};

export default async function EventPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string; eventId: string }>;
  searchParams: Promise<{ page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId } = await params;
  if (!/^[0-9]{1,18}$/.test(eventId)) notFound();
  const { page } = await searchParams;
  const currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  const { federationEventByFederationAndId: event } = await executeGraphql(
    FederationEventDocument,
    {
      federation,
      eventId,
      withCompetitions: true,
      competitionsOffset: Math.min((currentPage - 1) * 100, 2_147_483_647),
    },
  );
  if (!event?.competitions) notFound();
  const competitions = event.competitions;
  if (currentPage > Math.max(1, Math.ceil(competitions.totalCount / 100))) notFound();
  const sources = auth.isSystemAdmin
    ? await runQuery(browseSourceFrontiers, {
        federation,
        eventKey: event.externalId,
        competitionKey: null,
      })
    : [];
  const title = event.name || event.externalId;
  const path = `/federated/${federation}/${eventId}`;
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
        <dt>date</dt>
        <dd>
          {numericDateWithYearFormatter.format(new Date(event.startDate))}
          {event.endDate && event.endDate !== event.startDate && (
            <> – {numericDateWithYearFormatter.format(new Date(event.endDate))}</>
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
      <h2 className="mt-4 mb-2 text-lg font-bold">Competitions</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'competition ID',
                'category',
                'type',
                'date',
                'check-in end',
                'registrations',
                'results',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitions.nodes.map((competition) => (
              <tr key={competition.id} className="border-t border-neutral-4">
                <td className="px-2 py-1">{competition.externalId}</td>
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${path}/${competition.id}`}
                    prefetch={false}
                  >
                    {competition.category?.name || competition.externalId}
                  </Link>
                </th>
                <td className="px-2 py-1">
                  {competition.competitionType?.toLowerCase()}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {numericDateWithYearFormatter.format(new Date(competition.startDate))}
                </td>
                <td className="px-2 py-1">{competition.checkInEnd}</td>
                <td className="px-2 py-1">{competition.entries.totalCount}</td>
                <td className="px-2 py-1">{competition.results.totalCount}</td>
              </tr>
            ))}
            {competitions.nodes.length === 0 && (
              <tr>
                <td colSpan={7} className="px-2 py-1">
                  No competitions
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={competitions.totalCount}
        limit={100}
        page={currentPage}
        href={(page) => `${path}?page=${page}`}
      />
      {sources.length > 0 && (
        <section>
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
              — fetch: {source.fetch_status}, process: {source.process_status}
            </p>
          ))}
        </section>
      )}
    </Layout>
  );
}
