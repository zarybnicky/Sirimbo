import type { ReactNode } from 'react';
import { DefinitionList } from '@/ui/DefinitionList';
import { DotSeparated } from '@/ui/DotSeparated';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import type { OfficialRole } from '@/graphql';
import { LocalPersonLinks } from '@/ui/federation-links';
import { EntityLink } from '@/ui/EntityLink';
import { formatDiscipline } from '@/ui/csts';
import { formatOfficialRole } from '@/ui/format';
import { compareNaturalText } from '@/lib/compare';

export function Officials({
  officials,
  showJudges = false,
}: {
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
        (a.person?.canonicalName ?? '').localeCompare(b.person?.canonicalName ?? ''),
    );
  const judges = showJudges
    ? [
        ...new Map(
          officials.filter((x) => x.role === 'ADJUDICATOR').map((x) => [x.personId, x]),
        ).values(),
      ].toSorted(
        (a, b) =>
          compareNaturalText(a.judgeLabel ?? '', b.judgeLabel ?? '') ||
          (a.person?.canonicalName ?? '').localeCompare(b.person?.canonicalName ?? ''),
      )
    : [];
  if (rows.length === 0 && judges.length === 0) return null;
  const roles = Map.groupBy(rows, (x) => x.role);

  return (
    <div className="mt-3 space-y-3">
      {judges.length > 0 && (
        <>
          <h2 className="mb-2 text-lg font-bold">Porota</h2>
          <DefinitionList
            items={judges.map((x): [string, ReactNode] => [
              x.judgeLabel ?? '',
              <OfficialName key={x.personId} official={x} />,
            ])}
          />
        </>
      )}

      {roles.size > 0 && (
        <>
          <h2 className="mb-2 text-lg font-bold">Funkcionáři</h2>
          <DefinitionList
            className="text-sm tabular space-y-1"
            items={[...roles].map(([role, members]): [string, ReactNode] => [
              formatOfficialRole(role),
              <ul key={role}>
                {members.map((x) => (
                  <li key={`${x.personId}:${x.discipline ?? ''}`}>
                    <OfficialName official={x} />
                    <DotSeparated className="ml-0.5 flex text-xs text-neutral-11">
                      {x.judgeLabel}
                      {formatDiscipline(x.discipline)}
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

export function OfficialName({
  official,
  compact = true,
}: {
  official: { personId: string; person?: OfficialPersonFragment | null };
  compact?: boolean;
}) {
  const person = official.person;
  const name = person?.canonicalName || official.personId;

  return (
    <span className={compact ? 'inline-flex items-baseline gap-2' : 'contents'}>
      {person ? (
        <EntityLink
          type="federation-person"
          entity={person}
          className={compact ? 'font-medium text-accent-11' : 'text-accent-11'}
        >
          {name}
        </EntityLink>
      ) : (
        name
      )}
      {!!person?.peopleList.length && (
        <span className={compact ? 'text-xs text-neutral-11' : 'ml-2 text-sm'}>
          V klubu: <LocalPersonLinks people={person.peopleList} />
        </span>
      )}
    </span>
  );
}
