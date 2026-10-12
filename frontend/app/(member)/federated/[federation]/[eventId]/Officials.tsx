import type { OfficialPersonFragment } from '@/graphql/Federation';
import type { OfficialRole } from '@/graphql';
import { FederationPersonLink, LocalPersonLinks } from '@/ui/federation-links';

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
  const showMatches = rows.some((official) => official.person?.people.totalCount);

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Officials</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              <th scope="col" className="px-2 py-1">
                role
              </th>
              <th scope="col" className="px-2 py-1">
                person
              </th>
              {showMatches && (
                <th scope="col" className="px-2 py-1">
                  local matches
                </th>
              )}
              {showLabel && (
                <th scope="col" className="px-2 py-1">
                  label
                </th>
              )}
              {showDiscipline && (
                <th scope="col" className="px-2 py-1">
                  discipline
                </th>
              )}
              {showGrade && (
                <th scope="col" className="px-2 py-1">
                  grade
                </th>
              )}
            </tr>
          </thead>
          <tbody>
            {rows.map((official) => (
              <tr
                key={`${official.personId}:${official.role}:${official.discipline ?? ''}`}
                className="border-t border-neutral-4"
              >
                <td className="px-2 py-1">
                  {official.role.toLowerCase().replaceAll('_', ' ')}
                </td>
                <th scope="row" className="px-2 py-1 font-normal">
                  {official.person ? (
                    <FederationPersonLink
                      federation={federation}
                      externalId={official.person.externalId}
                      className="text-accent-11"
                    >
                      {official.person.canonicalName ||
                        [official.person.firstName, official.person.lastName]
                          .filter(Boolean)
                          .join(' ') ||
                        official.personId}
                    </FederationPersonLink>
                  ) : (
                    official.personId
                  )}
                </th>
                {showMatches && (
                  <td className="px-2 py-1">
                    {official.person && (
                      <LocalPersonLinks
                        federation={federation}
                        externalId={official.person.externalId}
                        people={official.person.people}
                      />
                    )}
                  </td>
                )}
                {showLabel && <td className="px-2 py-1">{official.judgeLabel}</td>}
                {showDiscipline && <td className="px-2 py-1">{official.discipline}</td>}
                {showGrade && <td className="px-2 py-1">{official.grade}</td>}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </>
  );
}
