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
    .filter((official) => showJudges || official.role !== 'ADJUDICATOR')
    .toSorted(
      (a, b) =>
        a.role.localeCompare(b.role) ||
        (a.person?.canonicalName ?? a.personId).localeCompare(
          b.person?.canonicalName ?? b.personId,
        ) ||
        (a.discipline ?? '').localeCompare(b.discipline ?? ''),
    );
  if (rows.length === 0) return null;
  const showDiscipline = rows.some((official) => official.discipline);
  const showGrade = rows.some((official) => official.grade);
  const showLabel = rows.some((official) => official.judgeLabel);
  const showMatches = rows.some((official) => official.person?.peopleList.length);

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Officials</h2>
      <Table
        columns={[
          'role',
          'person',
          showMatches && 'local matches',
          showLabel && 'label',
          showDiscipline && 'discipline',
          showGrade && 'grade',
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
            {showMatches && (
              <td>
                {official.person && (
                  <LocalPersonLinks people={official.person.peopleList} />
                )}
              </td>
            )}
            {showLabel && <td>{official.judgeLabel}</td>}
            {showDiscipline && <td>{official.discipline}</td>}
            {showGrade && <td>{official.grade}</td>}
          </tr>
        ))}
      </Table>
    </>
  );
}
