import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import {
  browseFederatedPerson,
  browsePersonCompetitors,
  browsePersonLocalMatches,
  browsePersonFrontiers,
} from '../../../federated.queries';

export const metadata = {
  title: 'Federated person',
  robots: { index: false, follow: false },
};

export default async function FederatedPersonPage({
  params,
}: {
  params: Promise<{ federation: string; id: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation, id: externalId } = await params;
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const id = `${federation}:${externalId}`;
  const [person] = await runQuery(browseFederatedPerson, { federation, id });
  if (!person) notFound();
  const [competitors, matches, sources] = await Promise.all([
    runQuery(browsePersonCompetitors, { id }),
    runQuery(browsePersonLocalMatches, { id }),
    runQuery(browsePersonFrontiers, { federation, key: person.external_id }),
  ]);
  const base = `/admin/federated/${federation}`;
  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={person.name || person.id}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: person.federation_name, href: base },
          { label: person.name || person.id },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>person ID</dt>
        <dd>{person.id}</dd>
        <dt>date of birth</dt>
        <dd>{person.dob ? numericDateWithYearFormatter.format(person.dob) : '-'}</dd>
        <dt>gender</dt>
        <dd>{person.gender || '-'}</dd>
        <dt>nationality</dt>
        <dd>{person.nationality || '-'}</dd>
        <dt>age group</dt>
        <dd>{person.age_group || '-'}</dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Local matches</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['person', 'active memberships'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {matches.map((match) => (
              <tr key={match.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`/clenove/${match.id}`}
                  >
                    {match.name}
                  </Link>
                </th>
                <td className="px-2 py-1">
                  {match.tenants?.join(', ') || 'No active membership'}
                </td>
              </tr>
            ))}
            {matches.length === 0 && (
              <tr>
                <td colSpan={2} className="px-2 py-1">
                  No local matches
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <h2 className="mt-4 mb-2 text-lg font-bold">Competitors</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['competitor', 'competitor ID', 'type', 'role'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitors.map((competitor) => (
              <tr key={competitor.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${base}/competitors/${competitor.external_id}`}
                  >
                    {competitor.name || competitor.id}
                  </Link>
                </th>
                <td className="px-2 py-1">{competitor.id}</td>
                <td className="px-2 py-1">{competitor.competitor_type}</td>
                <td className="px-2 py-1">{competitor.role}</td>
              </tr>
            ))}
            {competitors.length === 0 && (
              <tr>
                <td colSpan={4} className="px-2 py-1">
                  No competitors
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
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
      {sources.length === 0 && <p>No matching frontiers</p>}
    </Layout>
  );
}
