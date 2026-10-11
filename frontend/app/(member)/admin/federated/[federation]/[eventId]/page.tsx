import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import {
  browseEvent,
  browseCompetitions,
  browseSourceFrontiers,
} from '../../federated.queries';

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
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation, eventId } = await params;
  if (!/^[0-9]{1,18}$/.test(eventId)) notFound();
  const [event] = await runQuery(browseEvent, { federation, eventId });
  if (!event) notFound();
  const [competitions, sources] = await Promise.all([
    runQuery(browseCompetitions, { eventId }),
    runQuery(browseSourceFrontiers, {
      federation,
      eventKey: event.external_id,
      competitionKey: null,
    }),
  ]);
  const title = event.name || event.external_id;
  const path = `/admin/federated/${federation}/${eventId}`;
  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={title}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: event.federation_name, href: `/admin/federated/${federation}` },
          { label: title },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>event ID</dt>
        <dd>{event.external_id}</dd>
        <dt>date</dt>
        <dd>
          {numericDateWithYearFormatter.format(event.start_date)}
          {event.end_date && event.end_date.getTime() !== event.start_date.getTime() && (
            <> – {numericDateWithYearFormatter.format(event.end_date)}</>
          )}
        </dd>
        <dt>venue</dt>
        <dd>
          {[
            event.location,
            event.street_address,
            event.city,
            event.postal_code,
            event.country,
          ]
            .filter(Boolean)
            .join(', ') || '-'}
        </dd>
      </dl>
      <h2 className="text-xl font-bold">Competitions</h2>
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
            {competitions.map((competition) => (
              <tr key={competition.id} className="border-t border-neutral-4">
                <td className="px-2 py-1">{competition.external_id}</td>
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${path}/${competition.id}`}
                    prefetch={false}
                  >
                    {competition.category}
                  </Link>
                </th>
                <td className="px-2 py-1">{competition.competition_type}</td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {numericDateWithYearFormatter.format(competition.start_date)}
                </td>
                <td className="px-2 py-1">{competition.check_in_end}</td>
                <td className="px-2 py-1">{competition.registrations}</td>
                <td className="px-2 py-1">{competition.results}</td>
              </tr>
            ))}
            {competitions.length === 0 && (
              <tr>
                <td colSpan={7} className="px-2 py-1">
                  No competitions
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {sources.length > 0 && (
        <section>
          <h2 className="text-xl font-bold">Crawler</h2>
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
