import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { ExternalLink } from 'lucide-react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Competitors } from './Competitors';
import { formatDateOnly } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationPersonDocument } from '@/graphql/Federation';
import { CrawlerTargetsDocument } from '@/graphql/Crawler';

type Props = {
  params: Promise<{ federation: string; id: string }>;
};

const getPerson = cache(async (federation: string, externalId: string) => {
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const { federationPersonByFederationAndExternalId: person } = await executeGraphql(
    FederationPersonDocument,
    {
      federation,
      externalId,
    },
  );
  if (!person) notFound();
  return {
    person,
    name:
      person.canonicalName ||
      [person.firstName, person.lastName].filter(Boolean).join(' ') ||
      person.id,
  };
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, id } = await params;
  const { person, name } = await getPerson(federation, id);
  return {
    title: `${name} · ${person.federationByFederation?.name ?? federation}`,
    robots: { index: false, follow: false },
  };
}

export default async function FederatedPersonPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, id: externalId } = await params;
  const { person, name } = await getPerson(federation, externalId);
  const crawler = auth.isSystemAdmin
    ? await executeGraphql(CrawlerTargetsDocument, {
        condition: { federation, kind: 'member', key: person.externalId },
      })
    : null;
  const sources = crawler?.crawlerTargets?.nodes ?? [];
  const matches = person.people.nodes;
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
        {federation === 'csts' && (
          <>
            <dt>source</dt>
            <dd>
              <a
                href={`https://www.csts.cz/dancesport/evidence/lide/${person.externalId}/osobni_udaje`}
                target="_blank"
                rel="noreferrer"
                className="inline-flex items-center gap-1 text-accent-11 underline"
              >
                ČSTS profile
                <ExternalLink className="size-3.5" aria-hidden="true" />
              </a>
            </dd>
          </>
        )}
        <dt>date of birth</dt>
        <dd>{person.dob ? formatDateOnly(person.dob) : '-'}</dd>
        <dt>gender</dt>
        <dd>{person.gender?.toLowerCase() || '-'}</dd>
        <dt>nationality</dt>
        <dd>{person.nationality || '-'}</dd>
        <dt>age group</dt>
        <dd>{person.ageGroup || '-'}</dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Licences</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['kind', 'discipline', 'grade', 'status', 'valid until'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {person.licenses.nodes.map((license) => (
              <tr
                key={`${license.kind}:${license.discipline}`}
                className="border-t border-neutral-4"
              >
                <th scope="row" className="px-2 py-1 font-normal">
                  {license.kind.toLowerCase().replaceAll('_', ' ')}
                </th>
                <td className="px-2 py-1">
                  {license.discipline.toLowerCase().replaceAll('_', ' ')}
                </td>
                <td className="px-2 py-1">{license.grade || '-'}</td>
                <td className="px-2 py-1">{license.status.toLowerCase()}</td>
                <td className="px-2 py-1 whitespace-nowrap tabular-nums">
                  {license.validUntil ? formatDateOnly(license.validUntil) : '-'}
                </td>
              </tr>
            ))}
            {person.licenses.nodes.length === 0 && (
              <tr>
                <td colSpan={5} className="px-2 py-1">
                  No licences
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {matches.length > 0 && (
        <>
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
              </tbody>
            </table>
          </div>
        </>
      )}
      <Competitors federation={federation} externalId={externalId} />
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
