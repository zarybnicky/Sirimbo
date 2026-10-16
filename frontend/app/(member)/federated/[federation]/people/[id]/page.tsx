import { DefinitionList } from '@/ui/DefinitionList';
import { Table } from '@/ui/Table';
import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { ExternalLink } from 'lucide-react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { Competitors } from './Competitors';
import { OfficialAssignments } from './OfficialAssignments';
import { formatDateOnly } from '@/ui/format';
import { formatAgeGroup } from '@/ui/csts';
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
    { federation, externalId },
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

  return (
    <Layout requireUser>
      <PageHeader
        title={name}
        breadcrumbLabel="Osoba"
        breadcrumbs={[
          { label: 'Organizace', href: '/federated' },
          {
            label: person.federationByFederation?.name ?? federation,
            href: `/federated/${federation}?view=people`,
          },
        ]}
      />

      <DefinitionList
        items={{
          ID: person.externalId,
          Zdroj: federation === 'csts' && (
            <a
              href={`https://www.csts.cz/dancesport/evidence/lide/${person.externalId}/osobni_udaje`}
              target="_blank"
              rel="noreferrer"
              className="inline-flex items-center gap-1 text-accent-11 underline"
            >
              ČSTS profile
              <ExternalLink className="size-3.5" aria-hidden="true" />
            </a>
          ),
          'Datum narození': person.dob && formatDateOnly(person.dob),
          Pohlaví: person.gender?.toLowerCase(),
          Národnost: person.nationality,
          Věk: person.ageGroup && formatAgeGroup(person.ageGroup),
        }}
      />

      {person.peopleList.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">V klubu</h2>
          <Table columns={['person', 'active affiliations']}>
            {person.peopleList.map((match) => (
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
                    .join(', ') || 'Mimo jakýkoli klub'}
                </td>
              </tr>
            ))}
          </Table>
        </>
      )}

      <Competitors federation={federation} externalId={externalId} />

      <h2 className="mt-4 mb-2 text-lg font-bold">Licence</h2>
      <Table
        columns={['Druh', 'Disciplína', 'Třída', 'Stav', 'Platná do']}
        empty="Žádné licence"
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

      {(person.eventOfficialAssignments.totalCount > 0 ||
        person.competitionOfficialAssignments.totalCount > 0) && (
        <OfficialAssignments federation={federation} id={person.id} />
      )}
      <Sources
        federation={federation}
        condition={{ federation, kind: 'member', key: person.externalId }}
      />
    </Layout>
  );
}
