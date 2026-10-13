import { Table } from '@/ui/Table';
import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { ExternalLink } from 'lucide-react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Competitors } from './Competitors';
import { OfficialAssignments } from './OfficialAssignments';
import { formatDateOnly } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationPersonDocument } from '@/graphql/Federation';
import { Sources } from '../../Sources';

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
      <Table
        columns={['kind', 'discipline', 'grade', 'status', 'valid until']}
        empty="No licences"
      >
        {person.licenses.nodes.map((license) => (
          <tr
            key={`${license.kind}:${license.discipline}`}
            className="border-t border-neutral-4"
          >
            <th scope="row">{license.kind.toLowerCase().replaceAll('_', ' ')}</th>
            <td>{license.discipline.toLowerCase().replaceAll('_', ' ')}</td>
            <td>{license.grade || '-'}</td>
            <td>{license.status.toLowerCase()}</td>
            <td className="whitespace-nowrap tabular-nums">
              {license.validUntil ? formatDateOnly(license.validUntil) : '-'}
            </td>
          </tr>
        ))}
      </Table>
      {matches.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Local matches</h2>
          <Table columns={['person', 'active affiliations']}>
            {matches.map((match) => (
              <tr key={match.id} className="border-t border-neutral-4">
                <th scope="row">
                  <Link
                    className="text-accent-11 underline"
                    href={`/clenove/${match.id}`}
                  >
                    {match.name}
                  </Link>
                </th>
                <td>
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
          </Table>
        </>
      )}
      <Competitors federation={federation} externalId={externalId} />
      {(person.eventOfficialAssignments.totalCount > 0 ||
        person.competitionOfficialAssignments.totalCount > 0) && (
        <OfficialAssignments federation={federation} id={person.id} />
      )}
      {auth.isSystemAdmin && (
        <Sources
          federation={federation}
          condition={{ federation, kind: 'member', key: person.externalId }}
        />
      )}
    </Layout>
  );
}
