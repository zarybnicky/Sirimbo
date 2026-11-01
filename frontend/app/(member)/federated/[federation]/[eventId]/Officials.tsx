import type { ReactNode } from 'react';
import { DefinitionList } from '@/ui/DefinitionList';
import { DotSeparated } from '@/ui/DotSeparated';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import type { OfficialRole } from '@/graphql';
import { FederationLink, LocalPersonLinks } from '@/ui/federation-links';
import { formatOfficialRole } from '@/ui/format';

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
    .filter((x) => x.role !== 'ADJUDICATOR')
    .toSorted(
      (a, b) =>
        a.role.localeCompare(b.role) ||
        (a.person?.canonicalName ?? '').localeCompare(b.person?.canonicalName ?? '') ||
        (a.discipline ?? '').localeCompare(b.discipline ?? ''),
    );
  const judges = showJudges
    ? [
        ...new Map(
          officials.filter((x) => x.role === 'ADJUDICATOR').map((x) => [x.personId, x]),
        ).values(),
      ].toSorted((a, b) =>
        (a.person?.canonicalName ?? '').localeCompare(b.person?.canonicalName ?? ''),
      )
    : [];
  if (rows.length === 0 && judges.length === 0) return null;
  const roles = Map.groupBy(rows, (x) => x.role);

  return (
    <div className="mt-3 space-y-3">
      {judges.length > 0 && (
        <>
          <h2 className="mb-2 text-lg font-bold">
            Porota
          </h2>
          <ul className="space-y-1">
            {judges.map((x) => (
              <li key={x.personId}>
                <OfficialName federation={federation} official={x} />
              </li>
            ))}
          </ul>
        </>
      )}

      {roles.size > 0 && (
        <>
          <h2 className="mb-2 text-lg font-bold">
            Funkcionáři
          </h2>
          <DefinitionList className="text-sm tabular space-y-1"
            items={[...roles].map(([role, members]): [string, ReactNode] => [
              formatOfficialRole(role),
              <ul key={role}>
                {members.map((x) => (
                  <li key={`${x.personId}:${x.discipline ?? ''}`}>
                    <OfficialName federation={federation} official={x} />
                    <DotSeparated className="ml-0.5 flex text-xs text-neutral-11">
                      {x.judgeLabel}
                      {x.discipline}
                    </DotSeparated>
                  </li>
                ))}
              </ul>,
            ])}
          />
        </>
      )}
    </div>
  );
}

function OfficialName({
  federation,
  official,
}: {
  federation: string;
  official: { personId: string; person?: OfficialPersonFragment | null };
}) {
  const person = official.person;
  const name =
    person?.canonicalName ||
    [person?.firstName, person?.lastName].filter(Boolean).join(' ') ||
    official.personId;

  return (
    <span className="inline-flex items-baseline gap-2">
      {person ? (
        <FederationLink
          entity="people"
          federation={federation}
          externalId={person.externalId}
          className="font-medium text-accent-11"
        >
          {name}
        </FederationLink>
      ) : (
        name
      )}
      {!!person?.peopleList.length && (
        <span className="text-xs text-neutral-11">
          V klubu: <LocalPersonLinks people={person.peopleList} />
        </span>
      )}
    </span>
  );
}
