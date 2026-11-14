'use client';

import { PersonListDocument } from '@/graphql/Person';
import { CohortListDocument } from '@/graphql/Cohorts';
import { useTenantId } from '@/lib/auth';
import { CourseDocument } from '@/starlet/graphql/Query';
import { fetchStarlet } from '@/starlet/query';
import { useAtomValue } from 'jotai';
import { useMemo, useState } from 'react';
import { useAsync } from 'react-async-hook';
import { useQuery } from 'urql';
import { EntityLink } from '@/ui/EntityLink';
import { ListHeader } from '@/ui/ListHeader';
import { SortButton } from '@/ui/SortButton';
import { Table } from '@/ui/Table';
import { compareText } from '@/lib/compare';
import { slugify } from '@/lib/slugify';
import { parseAsBoolean, useQueryState } from 'nuqs';
import {
  comparePeople,
  groupRegistrations,
  genderLabels,
  type ComparisonPerson,
} from './person-comparison';
import { starletSettingsAtom, starletTokenAtom } from './state';

export function PersonComparisonPreview() {
  const tenantId = useTenantId();
  return <ComparisonPreview key={tenantId} />;
}

function ComparisonPreview() {
  const token = useAtomValue(starletTokenAtom);
  const { courses } = useAtomValue(starletSettingsAtom);
  const [search, setSearch] = useState('');
  const [showAll, setShowAll] = useQueryState('all', parseAsBoolean.withDefault(false));
  const [sort, setSort] = useState<{
    column: 'source' | 'database';
    descending: boolean;
  }>({ column: 'source', descending: false });
  const [people] = useQuery({
    query: PersonListDocument,
    variables: { condition: { membership: { states: [] } } },
  });
  const [cohorts] = useQuery({ query: CohortListDocument });
  const source = useAsync(async () => {
    if (!token?.auth_ok) return [];
    const registrations: ComparisonPerson[] = [];
    for (const course of courses) {
      const result = await fetchStarlet(CourseDocument, { key: course.id });
      if (!result.course) throw new Error(`Kurz ${course.code} se nepodařilo načíst.`);
      for (const student of result.students ?? []) {
        if (!student) continue;
        registrations.push({
          id: `${course.id}:${student.key}`,
          firstName: student.name?.trim() ?? '',
          lastName: student.surname?.trim() ?? '',
          birthYear: student.year ?? null,
          gender: student.sex ? (student.sex === 'FEMALE' ? 'WOMAN' : 'MAN') : null,
          email: student.email?.trim().toLowerCase() || null,
          phone: student.phone?.replaceAll(' ', '').replace(/^\+420/, '') || null,
          externalIds: student.ref_key ? [student.ref_key] : [],
          groups: [course.code],
          course: `${course.code} · ${course.name}`,
          street: student.street?.trim() || null,
          orientationNumber: student.street_no?.trim() || null,
          city: student.city?.trim() || null,
          postalCode: student.post_code?.replaceAll(' ', '') || null,
        });
      }
    }
    return groupRegistrations(registrations);
  }, [token, courses]);

  const database = useMemo(() => {
    const managedIds = new Map(
      cohorts.data?.cohortsList?.map((c) => [c.id, !!c.externalIds?.length]),
    );
    return (people.data?.people?.nodes ?? [])
      .toSorted((a, b) => Number(a.isExternal) - Number(b.isExternal))
      .map((person): ComparisonPerson => {
        let keepReason: string | undefined;
        if (person.isAdmin) keepReason = 'správce';
        else if (person.isTrainer) keepReason = 'trenér';
        else if (!person.isMember) keepReason = 'nečlen';
        else if (!person.externalIds?.length) keepReason = 'bez vazby na evidenci';
        else if (!person.cohortIds?.some((id) => id && managedIds.get(id)))
          keepReason = 'mimo skupiny evidence';
        return {
          id: person.id,
          isFormer: person.isExternal,
          isMember: !!person.isMember,
          keepReason,
          unmatchedAction: keepReason
            ? undefined
            : person.cohortIds?.some((id) => id && managedIds.get(id) === false)
              ? 'Ukončit skupiny evidence'
              : 'Archivovat',
          firstName: person.firstName,
          lastName: person.lastName,
          birthYear: person.birthDate ? Number(person.birthDate.slice(0, 4)) : null,
          birthDate: person.birthDate,
          gender: person.gender,
          email: person.email?.trim().toLowerCase() || null,
          phone: person.phone?.replaceAll(' ', '').replace(/^\+420/, '') || null,
          externalIds: person.externalIds?.filter((id): id is string => !!id) ?? [],
          street: person.address?.street ?? null,
          orientationNumber: person.address?.orientationNumber ?? null,
          city: person.address?.city ?? null,
          postalCode: person.address?.postalCode ?? null,
          groups: person.cohortMembershipsList
            .filter(
              (membership) =>
                membership.status === 'ACTIVE' &&
                membership.cohort &&
                managedIds.get(membership.cohort.id),
            )
            .flatMap((membership) => membership.cohort?.name ?? []),
        };
      });
  }, [people.data, cohorts.data]);
  const rows = useMemo(
    () => comparePeople(source.result ?? [], database),
    [source.result, database],
  );
  const visible = rows
    .filter((row) => showAll || row.actions.length > 0 || row.needsReview)
    .filter((row) =>
      slugify(
        [row.source, row.database]
          .map((x) => (x ? `${x.firstName} ${x.lastName}` : ''))
          .join(' '),
      ).includes(slugify(search)),
    )
    .toSorted((left, right) => {
      const a = left[sort.column] ?? left.source ?? left.database!;
      const b = right[sort.column] ?? right.source ?? right.database!;
      return (
        (compareText(a.lastName, b.lastName) ||
          compareText(a.firstName, b.firstName) ||
          compareText(a.id, b.id)) * (sort.descending ? -1 : 1)
      );
    });

  if (!token?.auth_ok) return <p>Pro porovnání se přihlaste do evidence.</p>;
  if (courses.length === 0) return <p>Vyberte kurzy pro porovnání.</p>;
  const busy = source.loading || people.fetching || cohorts.fetching;
  const error = source.error ?? people.error ?? cohorts.error;

  return (
    <div className="not-prose text-neutral-12">
      <ListHeader
        search={search}
        onSearchChange={setSearch}
        searchPlaceholder="Hledat osobu…"
        actions={
          <button
            type="button"
            role="switch"
            aria-label="Zobrazit všechny osoby"
            aria-checked={showAll}
            className="flex items-center gap-2 rounded-sm text-sm focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-9"
            onClick={() => setShowAll((value) => !value)}
          >
            <span>K řešení</span>
            <span
              aria-hidden
              className="inline-flex h-5 w-9 items-center rounded-full bg-accent-7 p-0.5"
            >
              <span
                className={`size-4 rounded-full bg-neutral-1 motion-safe:transition-transform ${showAll ? 'translate-x-4' : 'translate-x-0'}`}
              />
            </span>
            <span>Vše</span>
          </button>
        }
      />
      <Table
        columns={[
          <SortButton key="source" column="source" value={sort} onChange={setSort}>
            Evidence ({source.result?.length ?? 0})
          </SortButton>,
          <SortButton key="database" column="database" value={sort} onChange={setSort}>
            Databáze ({rows.filter((r) => r.database).length})
          </SortButton>,
          'Akce',
        ]}
        busy={busy}
        error={error}
        empty="Žádné osoby."
        className="[&_table]:table-fixed [&_thead_th:last-child]:w-28"
      >
        {!busy &&
          !error &&
          visible.map(
            ({ source, database, reason, needsReview, differences, actions }) => (
              <tr
                key={`${source?.id ?? ''}:${database?.id ?? ''}`}
                className="align-top"
                title={reason ?? (needsReview ? 'Nejasná shoda' : undefined)}
              >
                {[source, database].map((person, side) => {
                  if (!person) return <td key={side} />;
                  const name = `${person.lastName} ${person.firstName}`;
                  return (
                    <td key={side}>
                      {side === 1 ? (
                        <>
                          <EntityLink type="person" entity={{ id: person.id, name }} />{' '}
                          <span className="text-xs text-neutral-11">
                            {!source && reason}
                          </span>
                        </>
                      ) : (
                        <details>
                          <summary className="cursor-pointer">
                            {name}{' '}
                            <span className="text-xs text-neutral-11">
                              {person.groups.join(', ')}
                            </span>
                          </summary>
                          {person.registrations?.map((registration) => (
                            <div key={registration.id} className="my-2 text-xs">
                              <div className="font-medium">
                                {registration.course} · {registration.lastName}{' '}
                                {registration.firstName}
                              </div>
                              <dl className="grid grid-cols-[auto_1fr] gap-x-2">
                                {[
                                  ['Rok', registration.birthYear],
                                  [
                                    'Pohlaví',
                                    registration.gender &&
                                      genderLabels[registration.gender],
                                  ],
                                  ['E-mail', registration.email],
                                  ['Telefon', registration.phone],
                                  ['Ulice', registration.street],
                                  ['Číslo orientační', registration.orientationNumber],
                                  ['Obec', registration.city],
                                  ['PSČ', registration.postalCode],
                                  ['ID', registration.externalIds.join(', ')],
                                ]
                                  .filter(([, value]) => value)
                                  .map(([label, value]) => (
                                    <div key={label} className="contents">
                                      <dt className="text-neutral-11">{label}</dt>
                                      <dd className="wrap-anywhere">{value}</dd>
                                    </div>
                                  ))}
                              </dl>
                            </div>
                          ))}
                        </details>
                      )}
                      {differences
                        .filter(({ field }) => side === 1 || field !== 'groups')
                        .map(({ field, label, values }) => (
                          <div
                            key={field}
                            className="text-xs text-neutral-11 wrap-anywhere"
                          >
                            {label}: {values[side] || '—'}
                          </div>
                        ))}
                      {person.conflicts?.map((conflict) => (
                        <div key={conflict} className="text-xs text-danger-11">
                          {conflict}
                        </div>
                      ))}
                    </td>
                  );
                })}
                <td className="text-xs text-neutral-11">
                  {actions.map((action) => (
                    <div key={action}>{action}</div>
                  ))}
                </td>
              </tr>
            ),
          )}
      </Table>
    </div>
  );
}
