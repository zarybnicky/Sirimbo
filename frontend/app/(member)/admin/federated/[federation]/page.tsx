import Link from 'next/link';
import { notFound } from 'next/navigation';
import { z } from 'zod';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import { browseFederation, browseEventCount, browseEvents } from '../federated.queries';
import { Filters } from './Filters';

export const metadata = {
  title: 'Federation events',
  robots: { index: false, follow: false },
};

export default async function FederationPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string }>;
  searchParams: Promise<{ search?: string; date?: string; page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation } = await params;
  const { search = '', date = '', page } = await searchParams;
  if (date && !z.iso.date().safeParse(date).success) notFound();
  const [item] = await runQuery(browseFederation, { federation });
  if (!item) notFound();
  const filters = { federation, search, date };
  const [count] = await runQuery(browseEventCount, filters);
  const total = count?.total ?? 0;
  const currentPage = Math.min(
    Math.max(1, Number.parseInt(page ?? '1', 10) || 1),
    Math.max(1, Math.ceil(total / 50)),
  );
  const events = await runQuery(browseEvents, {
    ...filters,
    offset: (currentPage - 1) * 50,
  });
  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={item.name}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: item.name },
        ]}
      />
      <Filters />
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['date', 'event', 'event ID', 'location', 'country', 'competitions'].map(
                (label) => (
                  <th key={label} scope="col" className="px-2 py-1">
                    {label}
                  </th>
                ),
              )}
            </tr>
          </thead>
          <tbody>
            {events.map((event) => (
              <tr key={event.id} className="border-t border-neutral-4">
                <td className="px-2 py-1 whitespace-nowrap">
                  {numericDateWithYearFormatter.format(event.start_date)}
                  {event.end_date &&
                    event.end_date.getTime() !== event.start_date.getTime() && (
                      <> – {numericDateWithYearFormatter.format(event.end_date)}</>
                    )}
                </td>
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`/admin/federated/${federation}/${event.id}`}
                    prefetch={false}
                  >
                    {event.name || event.external_id}
                  </Link>
                </th>
                <td className="px-2 py-1">{event.external_id}</td>
                <td className="px-2 py-1">{event.location || event.city}</td>
                <td className="px-2 py-1">{event.country}</td>
                <td className="px-2 py-1">{event.competitions}</td>
              </tr>
            ))}
            {events.length === 0 && (
              <tr>
                <td colSpan={6} className="px-2 py-1">
                  No events found
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={total}
        limit={50}
        page={currentPage}
        href={(page) =>
          `/admin/federated/${federation}?${new URLSearchParams({ search, date, page: String(page) })}`
        }
      />
    </Layout>
  );
}
