'use client';

import {
  CreatePersonDocument,
  PersonListDocument,
  UpdatePersonDocument,
} from '@/graphql/Person';
import { CohortListDocument, SyncCohortMembershipsDocument } from '@/graphql/Cohorts';
import {
  CreateTenantMembershipDocument,
  UpdateTenantMembershipDocument,
} from '@/graphql/Memberships';
import { useTenantId } from '@/lib/auth';
import { CourseDocument } from '@/starlet/graphql/Query';
import { fetchStarlet } from '@/starlet/query';
import { useAtomValue } from 'jotai';
import { useMemo, useState } from 'react';
import { useAsync, useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { EntityLink } from '@/ui/EntityLink';
import { DefinitionList } from '@/ui/DefinitionList';
import { ListHeader } from '@/ui/ListHeader';
import { SortButton } from '@/ui/SortButton';
import { Table } from '@/ui/Table';
import { SubmitButton } from '@/ui/submit';
import { FormError } from '@/ui/form';
import { compareText } from '@/lib/compare';
import { slugify } from '@/lib/slugify';
import { parseAsBoolean, useQueryState } from 'nuqs';
import {
  comparePeople,
  groupRegistrations,
  genderLabels,
  actionLabels,
  type ComparisonPerson,
  type ComparisonRow,
} from './person-comparison';
import { starletSettingsAtom, starletTokenAtom } from './state';

export function PersonComparisonPreview() {
  const tenantId = useTenantId();
  const [activeId, setActiveId] = useState<string>();
  const token = useAtomValue(starletTokenAtom);
  const { courses } = useAtomValue(starletSettingsAtom);
  const [search, setSearch] = useState('');
  const [showAll, setShowAll] = useQueryState('all', parseAsBoolean.withDefault(false));
  const [sort, setSort] = useState<{
    column: 'source' | 'database';
    descending: boolean;
  }>({ column: 'source', descending: false });
  const [people, refetchPeople] = useQuery({
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
          membershipId: person.tenantMembershipsList.find((x) => x.tenantId === tenantId)
            ?.id,
          keepReason,
          unmatchedAction: keepReason
            ? undefined
            : person.cohortIds?.some((id) => id && managedIds.get(id) === false)
              ? 'endGroups'
              : 'archive',
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
          address: person.address ?? undefined,
          cohortIds: person.cohortIds?.filter((id): id is string => !!id) ?? [],
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
  }, [people.data, cohorts.data, tenantId]);
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
  const hasActions = visible.some((row) => row.actions.length > 0);
  const create = useMutation(CreatePersonDocument)[1];
  const update = useMutation(UpdatePersonDocument)[1];
  const createMembership = useMutation(CreateTenantMembershipDocument)[1];
  const updateMembership = useMutation(UpdateTenantMembershipDocument)[1];
  const syncCohorts = useMutation(SyncCohortMembershipsDocument)[1];
  const apply = useAsyncCallback(async (row: ComparisonRow) => {
    const { source, database, actions } = row;
    if (row.needsReview || actions.length === 0) return;
    setActiveId(source?.id ?? database?.id);
    const cohortIds = (database?.cohortIds ?? []).filter(
      (id) =>
        !cohorts.data?.cohortsList?.some((c) => c.id === id && c.externalIds?.length),
    );
    if (source && (actions.includes('groups') || actions.includes('create'))) {
      for (const name of source.groups) {
        const matches = cohorts.data?.cohortsList?.filter(
          (c) => c.name === name && c.externalIds?.length,
        );
        if (!matches?.length) throw new Error(`Nejdříve synchronizujte kurz ${name}.`);
        cohortIds.push(...matches.map((c) => c.id));
      }
    }
    try {
      let personId = database?.id;
      if (source) {
        const patch = {
          firstName: source.firstName,
          lastName: source.lastName,
          gender: source.gender ?? 'MAN',
          email: source.email,
          phone: source.phone,
          ...(!database?.birthDate && source.birthYear
            ? { birthDate: `${source.birthYear}-01-01` }
            : {}),
          externalIds: [...new Set(source.externalIds)],
          address: {
            street: source.street ?? '',
            orientationNumber: source.orientationNumber ?? '',
            city: source.city ?? '',
            postalCode: source.postalCode ?? '',
            conscriptionNumber: database?.address?.conscriptionNumber ?? '',
            region: database?.address?.region ?? '',
            district: database?.address?.district,
          },
        };
        if (!database) {
          const result = await create({
            input: {
              p: {
                ...patch,
                nationality: '203',
                prefixTitle: '',
                suffixTitle: '',
                bio: '',
              },
              isMember: true,
              joinDate: new Date().toISOString(),
            },
          });
          if (result.error) throw result.error;
          personId = result.data?.createPerson?.result?.id;
          if (!personId) throw new Error('Osobu se nepodařilo vytvořit.');
        }
        if (
          !database ||
          actions.some((x) => ['rename', 'details', 'externalIds'].includes(x))
        ) {
          // create_person omits the address, so it needs a separate update on creation.
          const result = await update({
            input: {
              id: personId!,
              patch: database ? patch : { address: patch.address },
            },
          });
          if (result.error) throw result.error;
        }
        if (actions.includes('renewMembership')) {
          const result = await createMembership({
            input: { tenantMembership: { personId: personId!, tenantId } },
          });
          if (result.error) throw result.error;
        }
      }
      if (actions.includes('archive') && database?.membershipId) {
        const result = await updateMembership({
          input: {
            id: database.membershipId,
            patch: { status: 'EXPIRED', until: new Date().toISOString() },
          },
        });
        if (result.error) throw result.error;
      } else if (actions.some((x) => ['create', 'groups', 'endGroups'].includes(x))) {
        const result = await syncCohorts({ input: { personId: personId!, cohortIds } });
        if (result.error) throw result.error;
      }
    } finally {
      refetchPeople({ requestPolicy: 'network-only' });
    }
  });

  if (!token?.auth_ok) return <p>Pro porovnání se přihlaste do evidence.</p>;
  if (courses.length === 0) return <p>Vyberte kurzy pro porovnání.</p>;
  const busy = source.loading || people.fetching || cohorts.fetching;
  const error = source.error ?? people.error ?? cohorts.error;

  return (
    <div className="not-prose text-neutral-12">
      <FormError error={apply.error} />
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
            <span>Nesynchronizované</span>
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
          hasActions && 'Akce',
        ]}
        busy={busy}
        error={error}
        empty="Žádné osoby."
      >
        {visible.map((row) => {
          const { source, database, reason, differences, actions } = row;
          return (
            <tr key={`${source?.id ?? ''}:${database?.id ?? ''}`} className="align-top">
              <td>
                {source && (
                  <>
                    <details>
                      <summary className="cursor-pointer">
                        {source.lastName} {source.firstName}{' '}
                        <span className="text-xs text-neutral-11">
                          {source.groups.join(', ')}
                        </span>
                      </summary>
                      {source.registrations?.map((reg) => (
                        <div key={reg.id} className="my-2 text-xs">
                          <div className="font-medium">
                            {reg.firstName} {reg.lastName} ({reg.course})
                          </div>
                          <DefinitionList
                            className="tabular gap-x-2 [&_dt]:text-neutral-11 [&_dd]:wrap-anywhere"
                            items={{
                              Rok: reg.birthYear,
                              Pohlaví: reg.gender && genderLabels[reg.gender],
                              'E-mail': reg.email,
                              Telefon: reg.phone,
                              Ulice: reg.street,
                              'Číslo orientační': reg.orientationNumber,
                              Obec: reg.city,
                              PSČ: reg.postalCode,
                              ID: reg.externalIds.join(', '),
                            }}
                          />
                        </div>
                      ))}
                    </details>
                    {differences
                      .filter(({ field }) => field !== 'groups')
                      .map(({ field, label, values }) => (
                        <div
                          key={field}
                          className="text-xs text-neutral-11 wrap-anywhere"
                        >
                          {label}: {values[0] || '—'}
                        </div>
                      ))}
                    {source.conflicts?.map((x) => (
                      <div key={x} className="text-xs text-danger-11">
                        {x}
                      </div>
                    ))}
                  </>
                )}
              </td>
              <td>
                {database && (
                  <>
                    <EntityLink
                      type="person"
                      entity={{
                        id: database.id,
                        name: `${database.lastName} ${database.firstName}`,
                      }}
                    />{' '}
                    <span className="text-xs text-neutral-11">{!source && reason}</span>
                    {differences.map(({ field, label, values }) => (
                      <div key={field} className="text-xs text-neutral-11 wrap-anywhere">
                        {label}: {values[1] || '—'}
                      </div>
                    ))}
                  </>
                )}
              </td>
              {hasActions && (
                <td className="w-px whitespace-nowrap text-xs text-neutral-11">
                  {actions.map((x) => (
                    <div key={x}>{actionLabels[x]}</div>
                  ))}
                  {actions.length > 0 && (
                    <SubmitButton
                      type="button"
                      size="xs"
                      variant="outline"
                      state={
                        activeId === (source?.id ?? database?.id)
                          ? apply.status
                          : undefined
                      }
                      disabled={busy || apply.loading || row.needsReview}
                      onClick={() => apply.execute(row)}
                    >
                      Použít
                    </SubmitButton>
                  )}
                </td>
              )}
            </tr>
          );
        })}
      </Table>
    </div>
  );
}
