import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationPersonDocument } from '@/graphql/Federation';
import { CrawlerTargetsDocument } from '@/graphql/Crawler';

export const metadata = {
  title: 'Federated person',
  robots: { index: false, follow: false },
};

export default async function FederatedPersonPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string; id: string }>;
  searchParams: Promise<{ page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, id: externalId } = await params;
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const { page } = await searchParams;
  const currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  const { federationPersonByFederationAndExternalId: person } = await executeGraphql(
    FederationPersonDocument,
    {
      federation,
      externalId,
      competitorsOffset: Math.min((currentPage - 1) * 100, 2_147_483_647),
    },
  );
  if (!person) notFound();
  const competitors = person.competitorComponents;
  if (currentPage > Math.max(1, Math.ceil(competitors.totalCount / 100))) notFound();
  const crawler = auth.isSystemAdmin
    ? await executeGraphql(CrawlerTargetsDocument, {
        condition: { federation, kind: 'member', key: person.externalId },
      })
    : null;
  const sources = crawler?.crawlerTargets?.nodes ?? [];
  const matches = person.people.nodes;
  const name =
    person.canonicalName ||
    [person.firstName, person.lastName].filter(Boolean).join(' ') ||
    person.id;
  const base = `/federated/${federation}`;
  return (
    <Layout requireUser>
      <PageHeader
        title={name}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: person.federationByFederation?.name ?? federation,
            href: `${base}?view=people`,
          },
          { label: name },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>person ID</dt>
        <dd>{person.id}</dd>
        <dt>date of birth</dt>
        <dd>
          {person.dob ? numericDateWithYearFormatter.format(new Date(person.dob)) : '-'}
        </dd>
        <dt>gender</dt>
        <dd>{person.gender?.toLowerCase() || '-'}</dd>
        <dt>nationality</dt>
        <dd>{person.nationality || '-'}</dd>
        <dt>age group</dt>
        <dd>{person.ageGroup || '-'}</dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Local matches</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['person', 'active affiliations'].map((label) => (
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
                  {[
                    ...new Set(
                      [
                        ...match.tenantMembershipsList,
                        ...match.tenantTrainersList,
                        ...match.tenantAdministratorsList,
                      ]
                        .map((affiliation) => affiliation.tenantName)
                        .filter((name) => name != null),
                    ),
                  ]
                    .toSorted((a, b) => a.localeCompare(b))
                    .join(', ') || 'No active affiliation'}
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
            {competitors.nodes.map(({ competitorId, competitor, role }) => (
              <tr key={competitorId} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {competitor ? (
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/competitors/${competitor.externalId}`}
                    >
                      {competitor.name || competitorId}
                    </Link>
                  ) : (
                    competitorId
                  )}
                </th>
                <td className="px-2 py-1">{competitorId}</td>
                <td className="px-2 py-1">{competitor?.competitorType.toLowerCase()}</td>
                <td className="px-2 py-1">{role.toLowerCase()}</td>
              </tr>
            ))}
            {competitors.nodes.length === 0 && (
              <tr>
                <td colSpan={4} className="px-2 py-1">
                  No competitors
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={competitors.totalCount}
        limit={100}
        page={currentPage}
        href={(page) => `${base}/people/${externalId}?page=${page}`}
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
                {federation}:member:{source.key}
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
