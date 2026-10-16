import { Table } from '@/ui/Table';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import type { OfficialRole } from '@/graphql';
import { FederationLink, LocalPersonLinks } from '@/ui/federation-links';

export function Officials({
  federation,
  officials,
  showJudges = false,
}: {
  federation: string;
  officials: {
    personId: string;
    person?: OfficialPersonFragment | null;
    role: OfficialRole;
    discipline?: string;
    grade?: string | null;
    judgeLabel?: string | null;
  }[];
  showJudges?: boolean;
}) {
  const rows = officials
    .filter((x) => showJudges || x.role !== 'ADJUDICATOR')
    .toSorted(
      (a, b) =>
        a.role.localeCompare(b.role) ||
        (a.person?.canonicalName ?? '').localeCompare(b.person?.canonicalName ?? '') ||
        (a.discipline ?? '').localeCompare(b.discipline ?? ''),
    );
  if (rows.length === 0) return null;
  const showDiscipline = rows.some((x) => x.discipline);
  const showGrade = rows.some((x) => x.grade);
  const showLabel = rows.some((x) => x.judgeLabel);
  const showLocalMatches = rows.some((x) => x.person?.peopleList.length);

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Funkcionáři</h2>
      <Table
        columns={[
          'Role',
          'Jméno',
          showLabel && 'Popis',
          showDiscipline && 'Disciplína',
          showGrade && 'Třída',
          showLocalMatches && 'V klubu',
        ]}
      >
        {rows.map((official) => (
          <tr
            key={`${official.personId}:${official.role}:${official.discipline ?? ''}`}
            className="border-t border-neutral-4"
          >
            <td>{official.role.toLowerCase().replaceAll('_', ' ')}</td>
            <th scope="row">
              {official.person ? (
                <FederationLink
                  entity="people"
                  federation={federation}
                  externalId={official.person.externalId}
                  className="text-accent-11"
                >
                  {official.person.canonicalName ||
                    [official.person.firstName, official.person.lastName]
                      .filter(Boolean)
                      .join(' ') ||
                    official.personId}
                </FederationLink>
              ) : (
                official.personId
              )}
            </th>
            {showLabel && <td>{official.judgeLabel}</td>}
            {showDiscipline && <td>{official.discipline}</td>}
            {showGrade && <td>{official.grade}</td>}
            {showLocalMatches && (
              <td>
                <LocalPersonLinks people={official.person?.peopleList} />
              </td>
            )}
          </tr>
        ))}
      </Table>
    </>
  );
}
