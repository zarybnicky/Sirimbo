import { fetchStarlet } from '@/starlet/query';
import { Student } from '@/starlet/graphql';
import { CourseDocument } from '@/starlet/graphql/Query';
import { slugify } from '@/lib/slugify';
import { type JSX, useEffect, useMemo, useState } from 'react';
import {
  CreatePersonDocument,
  PersonListDocument,
  PersonMembershipsDocument,
  UpdatePersonDocument,
} from '@/graphql/Person';
import { useClient, useMutation, useQuery } from 'urql';
import { SubmitButton } from '@/ui/submit';
import { useAtomValue } from 'jotai';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { isTruthy } from '@/lib/truthyFilter';
import { CohortListDocument, SyncCohortMembershipsDocument } from '@/graphql/Cohorts';
import { useAsyncCallback } from 'react-async-hook';
import {
  CreateTenantMembershipDocument,
  UpdateTenantMembershipDocument,
} from '@/graphql/Memberships';
import { useTenantId } from '@/lib/auth';
import { capitalize } from '@/ui/format';
import { EntityLink } from '../EntityLink';

type QueriedStudent = Pick<
  Student,
  | 'key'
  | 'course_key'
  | 'sex'
  | 'ref_key'
  | 'name'
  | 'surname'
  | 'email'
  | 'phone'
  | 'year'
  | 'street'
  | 'street_no'
  | 'city'
  | 'post_code'
> & {
  normal_name: string;
  course_name: string;
};
type CleanedCourse = { students: QueriedStudent[] };

type DeduplicatedStudent = {
  normal_name: string;
  name: string;
  surname: string;
  gender: 'MAN' | 'WOMAN';
  email: string | null;
  phone: string | null;
  year: number | null;
  street: string | null;
  street_no: string | null;
  city: string | null;
  post_code: string | null;
  keys: string[];
  course_keys: string[];
  course_names: string[];
  ref_keys: string[];
};

type Person = {
  id: string;
  name: string;
  firstName: string;
  lastName: string;
  gender: string;
  birthDate: string | null;
  email: string | null;
  phone: string | null;
  cohortIds: (string | null)[] | null;
  address: {
    city: string | null;
    region: string | null;
    street: string | null;
    district: string | null;
    postalCode: string | null;
    orientationNumber: string | null;
    conscriptionNumber: string | null;
  } | null;
  externalIds: (string | null)[] | null;
  isMember: boolean | null;
  isTrainer: boolean | null;
  isAdmin: boolean | null;
};

type PersonTask =
  | { type: 'create'; student: DeduplicatedStudent; cohortIds: string[] }
  | { type: 'update'; student: DeduplicatedStudent; person: Person; cohortIds: string[] }
  | { type: 'archive'; person: Person; cohortIds: string[] };

function personFieldsFromStudent(student: DeduplicatedStudent, person?: Person) {
  return {
    firstName: student.name,
    lastName: student.surname,
    gender: student.gender,
    email: student.email,
    phone: student.phone,
    ...(!person || (!person.birthDate && student.year)
      ? { birthDate: student.year ? `${student.year}-01-01` : null }
      : {}),
    externalIds: student.ref_keys,
    address: {
      ...person?.address,
      street: student.street ?? '',
      orientationNumber: student.street_no ?? '',
      city: student.city ?? '',
      postalCode: student.post_code ?? '',
      conscriptionNumber: person?.address?.conscriptionNumber ?? '',
      region: person?.address?.region ?? '',
    },
  };
}

export function PersonComparisonForm() {
  const client = useClient();
  const tenantId = useTenantId();
  const token = useAtomValue(starletTokenAtom);
  const { courses } = useAtomValue(starletSettingsAtom);
  const [{ data: personQuery }] = useQuery({
    query: PersonListDocument,
    variables: { condition: { membership: { state: 'CURRENT' } } },
  });
  const [{ data: formerPersonQuery }] = useQuery({
    query: PersonListDocument,
    variables: { condition: { membership: { state: 'FORMER' } } },
  });
  const [{ data: cohortQuery }] = useQuery({ query: CohortListDocument });
  const persons = useMemo(() => {
    const byId = new Map(
      [
        ...(personQuery?.people?.nodes ?? []),
        ...(formerPersonQuery?.people?.nodes ?? []),
      ].map((person) => [person.id, person]),
    );
    return [...byId.values()];
  }, [personQuery, formerPersonQuery]);

  const [coursesWithStudents, setCoursesWithStudents] = useState<CleanedCourse[]>([]);
  useEffect(() => {
    if (!token?.auth_ok) return;
    fetchCoursesWithStudents(courses.map(({ id }) => id)).then(setCoursesWithStudents);
  }, [token?.auth_ok, courses]);

  const [students, problematic] = useMemo(
    () => deduplicateStudents(coursesWithStudents),
    [coursesWithStudents],
  );
  const [tasks, views] = useMemo(
    () => compare(
      persons || [],
      students,
      cohortQuery?.cohortsList || [],
      problematic.flatMap(([, candidates]) => candidates),
    ),
    [persons, students, cohortQuery?.cohortsList, problematic],
  );

  const create = useMutation(CreatePersonDocument)[1];
  const update = useMutation(UpdatePersonDocument)[1];
  const createMembership = useMutation(CreateTenantMembershipDocument)[1];
  const updateMembership = useMutation(UpdateTenantMembershipDocument)[1];
  const syncCohorts = useMutation(SyncCohortMembershipsDocument)[1];

  const onSubmit = useAsyncCallback(async () => {
    for (const task of tasks) {
      const { cohortIds } = task;
      let personId: string;
      if (task.type === 'create') {
        const { student } = task;
        const res = await create({
          input: {
            p: {
              ...personFieldsFromStudent(student),
              nationality: '203',
              // create_person explicitly inserts these columns, bypassing their defaults.
              prefixTitle: '',
              suffixTitle: '',
              bio: '',
            },
            isMember: true,
            joinDate: new Date().toISOString(),
          },
        });
        if (res.error) throw res.error;
        const createdId = res.data?.createPerson?.result?.id;
        if (!createdId) continue;
        personId = createdId;
      } else if (task.type === 'update') {
        const { student, person } = task;
        personId = person.id;
        const updateResult = await update({
          input: {
            id: person.id,
            patch: personFieldsFromStudent(student, person),
          },
        });
        if (updateResult.error) throw updateResult.error;
        if (!person.isMember) {
          const membershipResult = await createMembership({
            input: { tenantMembership: { personId: person.id, tenantId } },
          });
          if (membershipResult.error) throw membershipResult.error;
        }
      } else {
        personId = task.person.id;
      }

      const syncResult = await syncCohorts({ input: { personId, cohortIds } });
      if (syncResult.error) throw syncResult.error;

      if (task.type === 'archive' && cohortIds.length === 0) {
        const membershipsResult = await client
          .query(PersonMembershipsDocument, { id: personId })
          .toPromise();
        if (membershipsResult.error) throw membershipsResult.error;
        const memberships = membershipsResult.data!;
        const currentMembership = memberships.person?.tenantMembershipsList.find(
          (x) => x.tenantId === tenantId && x.status === 'ACTIVE',
        );
        if (currentMembership) {
          const membershipResult = await updateMembership({
            input: {
              id: currentMembership.id,
              patch: {
                status: 'EXPIRED',
                until: new Date().toISOString(),
              },
            },
          });
          if (membershipResult.error) throw membershipResult.error;
        }
      }
    }
  });

  if (coursesWithStudents.length === 0) return null;

  return (
    <>
      <h3>Problémy při sjednocení přihlášek</h3>
      <ul>
        {problematic.map(([name, candidates]) => (
          <li key={name}>
            {name}
            {candidates.length > 0 && (
              <>
                {' - objeven v '}
                {candidates.map((x) => x.course_name).join(', ')}
                <ul>
                  {Object.entries(
                    diffObjectsList(candidates),
                  ).map((x) => (
                    <li key={x[0]}>
                      {x[0]}: {JSON.stringify(x[1])}
                    </li>
                  ))}
                </ul>
              </>
            )}
          </li>
        ))}
        {problematic.every((x) => x[1].length === 0) && (
          <li>✅ Všechno v pořádku, nikomu nechybí přístup</li>
        )}
      </ul>

      <h3>Osoby bez údajů</h3>
      <ul>
        {students
          .filter((x) => !x.email)
          .map((x) => (
            <li key={x.keys.join(',')}>
              {x.normal_name} ({x.course_names.join(', ')}): chybí e-mail
            </li>
          ))}
        {students
          .filter((x) => !x.phone)
          .map((x) => (
            <li key={x.keys.join(',')}>
              {x.normal_name} ({x.course_names.join(', ')}): chybí telefon
            </li>
          ))}
        {students
          .filter((x) => !x.year)
          .map((x) => (
            <li key={x.keys.join(',')}>
              {x.normal_name} ({x.course_names.join(', ')}): chybí rok narození
            </li>
          ))}
      </ul>

      <h3>Výsledné změny k synchronizaci</h3>
      <ul>
        {views}
        {views.length === 0 && <li>✅ Žádné úpravy nejsou potřeba</li>}
      </ul>

      {tasks.length > 0 && (
        <SubmitButton className="mb-2" action={onSubmit}>
          Synchronizovat
        </SubmitButton>
      )}
    </>
  );
}

function compare(
  people: Person[],
  students: DeduplicatedStudent[],
  cohorts: {
    id: string;
    name: string;
    isVisible: boolean;
    externalIds: (string | null)[] | null;
  }[] = [],
  unresolvedStudents: QueriedStudent[] = [],
) {
  const views: JSX.Element[] = [];
  const tasks: PersonTask[] = [];
  const managedCohorts = cohorts.filter((cohort) => cohort.externalIds?.length);

  const peopleByNormalName = new Map<string, Person[]>();
  const peopleByExternalId = new Map<string, Person>();
  for (const person of people) {
    const normalName = getNormalizedName(person.firstName, person.lastName);
    const people = peopleByNormalName.get(normalName) || [];
    people.push(person);
    peopleByNormalName.set(normalName, people);
    for (const externalId of person.externalIds ?? []) {
      if (externalId && !peopleByExternalId.has(externalId)) {
        peopleByExternalId.set(externalId, person);
      }
    }
  }

  const processedPeople = new Set<string>();
  for (const student of students) {
    const candidates = (peopleByNormalName.get(student.normal_name) || []).filter(
      (x) => !processedPeople.has(x.id),
    );

    const person =
      student.ref_keys
        .map((x) => peopleByExternalId.get(x))
        .find((x) => x && !processedPeople.has(x.id)) ??
      (candidates.length > 0 ? disambiguateCandidates(student, candidates) : undefined);

    const targetCohortIds = managedCohorts
      .filter((x) => student.course_names.includes(x.name))
      .map((x) => x.id);

    if (!person) {
      tasks.push({ type: 'create', student, cohortIds: targetCohortIds });

      views.push(
        <li key={student.ref_keys.join(',')} className="my-0">
          {student.normal_name} ({student.year})
          <ul className="my-0">
            <li className="mt-0">❌ Osoba bude vytvořena</li>
          </ul>
        </li>,
      );
      continue;
    }
    processedPeople.add(person.id);

    const birthYear = person.birthDate ? new Date(person.birthDate).getFullYear() : null;
    const courseList = new Set(student.course_names);
    const currentCohortIds = new Set(person.cohortIds || []);
    const unmanagedCohortIds = cohorts
      .filter((cohort) => !cohort.externalIds?.length && currentCohortIds.has(cohort.id))
      .map((cohort) => cohort.id);
    const cohortList = new Set(
      managedCohorts.filter((x) => currentCohortIds.has(x.id)).map((x) => x.name),
    );

    const patch = personFieldsFromStudent(student, person);
    const genderLabels: Record<string, string> = { MAN: 'Muž', WOMAN: 'Žena' };
    const changes = [
      ['Jméno', person.firstName, patch.firstName],
      ['Příjmení', person.lastName, patch.lastName],
      ['E-mail', person.email, patch.email],
      ['Telefon', person.phone, patch.phone],
      ['Pohlaví', genderLabels[person.gender] ?? person.gender, genderLabels[patch.gender]],
      ['Rok narození', birthYear?.toString(), patch.birthDate?.slice(0, 4) ?? birthYear?.toString()],
      ['Ulice', person.address?.street, patch.address.street],
      ['Číslo orientační', person.address?.orientationNumber, patch.address.orientationNumber],
      ['Obec', person.address?.city, patch.address.city],
      ['PSČ', person.address?.postalCode, patch.address.postalCode],
      [
        'Externí ID',
        [...new Set(person.externalIds?.filter(isTruthy))].toSorted().join(', '),
        [...new Set(patch.externalIds)].toSorted().join(', '),
      ],
      ['Kurzy', [...cohortList].toSorted().join(', '), [...courseList].toSorted().join(', ')],
    ].filter(([, before, after]) => (before ?? '') !== (after ?? ''));

    if (changes.length > 0) {
      const cohortIds = [...targetCohortIds, ...unmanagedCohortIds];
      tasks.push({ type: 'update', student, person, cohortIds });
      views.push(
        <li key={student.ref_keys.join(',')} className="my-0">
          {student.normal_name} ({student.year})
          <ul className="my-0">
            <li className="mt-0">
              ✅{' '}
              <EntityLink type="person" entity={person}>
                {person.name} ({birthYear})
              </EntityLink>
              {' - osoba bude aktualizována'}
              <ul className="my-0">
                {changes.map(([label, before, after]) => (
                  <li key={label} className="my-0">
                    {label}: {before || '(neuvedeno)'} → {after || '(neuvedeno)'}
                  </li>
                ))}
                {!person.isMember && <li className="my-0">Členství bude obnoveno</li>}
              </ul>
            </li>
          </ul>
        </li>,
      );
    }
  }
  const unresolvedNames = new Set(unresolvedStudents.map((x) => x.normal_name));
  const unresolvedRefKeys = new Set(unresolvedStudents.map((x) => x.ref_key ?? []));
  for (const person of people) {
    const currentCohortIds = new Set(person.cohortIds || []);
    if (
      processedPeople.has(person.id) ||
      unresolvedNames.has(getNormalizedName(person.firstName, person.lastName)) ||
      person.externalIds?.some((id) => id && unresolvedRefKeys.has(id)) ||
      person.isAdmin ||
      person.isTrainer ||
      !person.isMember ||
      !person.externalIds?.length ||
      !managedCohorts.some((x) => currentCohortIds.has(x.id))
    ) {
      continue;
    }
    const birthYear = new Date(person.birthDate || '1900-01-01').getFullYear();
    const unmanagedCohortIds = cohorts
      .filter((cohort) => !cohort.externalIds?.length && currentCohortIds.has(cohort.id))
      .map((cohort) => cohort.id);

    tasks.push({ type: 'archive', person, cohortIds: unmanagedCohortIds });
    views.push(
      <li key={person.id} className="my-0">
        ❌
        <ul className="my-0">
          <li className="mt-0">
            <EntityLink type="person" entity={person}>
              {person.name} ({birthYear})
            </EntityLink>
            {unmanagedCohortIds.length > 0
              ? 'Externě spravovaná členství budou ukončena. Osoba zůstane členem v ostatních skupinách.'
              : 'Osoba bude archivována'}
          </li>
        </ul>
      </li>,
    );
  }
  return [tasks, views] as const;
}

function disambiguateCandidates(student: DeduplicatedStudent, candidates: Person[]) {
  const byBirthYears = candidates.map(
    (x) => [new Date(x.birthDate || '1900-01-01').getFullYear(), x] as const,
  );
  const matchingBirthYear = byBirthYears.filter((x) => x[0] === student.year);
  const missingBirthYear = byBirthYears.filter((x) => x[0] === 1900);
  const matchesEmail = (x: Person) =>
    !!student.email && x.email?.trim().toLowerCase() === student.email;
  const matchingEmail = candidates.filter(matchesEmail);

  return (
    (student.year ? matchingBirthYear.find((x) => matchesEmail(x[1]))?.[1] : undefined) ??
    (student.year ? matchingBirthYear[0]?.[1] : undefined) ??
    (matchingEmail.length === 1 ? matchingEmail[0] : undefined) ??
    missingBirthYear.find((x) => matchesEmail(x[1]))?.[1] ??
    missingBirthYear[0]?.[1] ??
    (!student.year ? candidates[0] : undefined)
  );
}

async function fetchCoursesWithStudents(courses: string[]) {
  const result: CleanedCourse[] = [];

  for (const key of courses) {
    const { course, students } = await fetchStarlet(CourseDocument, { key });
    if (!course) continue;

    const cleanedStudents = [];
    for (const student of students || []) {
      if (!student) continue;

      const cleanedStudent: QueriedStudent = {
        normal_name: getNormalizedName(student.name, student.surname),
        course_name: course.code!,
        key: student.key,
        course_key: student.course_key,
        sex: student.sex,
        ref_key: student.ref_key,
        name: student.name?.trim() ?? null,
        surname: student.surname?.trim() ?? null,
        email: student.email?.trim().toLowerCase() ?? null,
        phone: student.phone?.replaceAll(' ', '').replace(/^\+420/, '') ?? null,
        year: student.year,
        street: student.street?.trim() ?? null,
        street_no: student.street_no?.trim() ?? null,
        city: student.city?.trim() ?? null,
        post_code: student.post_code?.replaceAll(' ', '') ?? null,
      };
      cleanedStudents.push(cleanedStudent);
    }
    result.push({ students: cleanedStudents });
  }
  return result;
}

function deduplicateStudents(courses: CleanedCourse[]) {
  const studentMap = new Map<string, QueriedStudent[]>();
  for (const { students } of courses) {
    for (const student of students) {
      const existingStudents = studentMap.get(student.normal_name) || [];
      existingStudents.push(student);
      studentMap.set(student.normal_name, existingStudents);
    }
  }

  const deduplicated: DeduplicatedStudent[] = [];
  const problematic: [string, QueriedStudent[]][] = [];

  for (const [normalName, candidates] of studentMap.entries()) {
    if (candidates.length === 0) continue;

    const birthYears = new Set(candidates.map((x) => x.year ?? []));
    const phones = new Set(candidates.map((x) => x.phone ?? []));
    const emails = new Set(candidates.map((x) => x.email ?? []));

    if (birthYears.size < 2) {
      if (phones.size < 2 && emails.size < 2) {
        // clean merge or a single candidate, pick first in case of address conflict
        deduplicated.push(mergeCandidates(candidates));
        if (candidates.length > 1)
          problematic.push([`${normalName}: více přihlášek studenta sloučeno`, []]);
      } else {
        problematic.push([
          `${normalName}: Stejný rok narození ale jiné kontaktní údaje, nemůžu sloučit`,
          candidates,
        ]);
      }
    } else if (birthYears.size >= phones.size && birthYears.size >= emails.size) {
      if (candidates.map((x) => x.year).some((x) => !x)) {
        // some birth year is falsy, bail
        problematic.push([
          `${normalName}: Podle roku narození jiní lidé, ale některým rok narození chybí, nemůžu rozdělit`,
          candidates,
        ]);
      } else {
        for (const discriminant of birthYears) {
          deduplicated.push(
            mergeCandidates(candidates.filter((x) => x.year === discriminant)),
          );
        }
        if (birthYears.size > 1)
          problematic.push([
            `${normalName}: konflikt vyřešen rozdělením podle roků narození (${[...birthYears].join(', ')})`,
            [],
          ]);
      }
    } else {
      problematic.push([
        `${normalName}: Konfliktní roky narození a kontaktní údaje, nemůžu rozdělit`,
        candidates,
      ]);
    }
  }

  deduplicated.sort((x, y) =>
    `${x.surname}, ${x.name}`.localeCompare(`${y.surname}, ${y.name}`),
  );

  return [deduplicated, problematic] as const;
}

function mergeCandidates(candidates: QueriedStudent[]): DeduplicatedStudent {
  const sex = candidates.map((x) => x.sex).find(isTruthy);
  return {
    normal_name: candidates.map((x) => x.normal_name).find(isTruthy)!,
    name: candidates.map((x) => x.name).find(isTruthy) || '?',
    surname: candidates.map((x) => x.surname).find(isTruthy) || '?',
    gender: sex === 'FEMALE' ? 'WOMAN' : 'MAN',
    email: candidates.map((x) => x.email).find(isTruthy) || null,
    phone: candidates.map((x) => x.phone).find(isTruthy) || null,
    year: candidates.map((x) => x.year).find(isTruthy) || null,
    street: candidates.map((x) => x.street).find(isTruthy) || null,
    street_no: candidates.map((x) => x.street_no).find(isTruthy) || null,
    city: candidates.map((x) => x.city).find(isTruthy) || null,
    post_code: candidates.map((x) => x.post_code).find(isTruthy) || null,
    keys: candidates.flatMap((x) => x.key ?? []),
    course_names: candidates.flatMap((x) => x.course_name ?? []),
    course_keys: candidates.flatMap((x) => x.course_key ?? []),
    ref_keys: candidates.flatMap((x) => x.ref_key ?? []),
  };
}

function getNormalizedName(name: string | null, surname: string | null): string {
  return `${capitalize(slugify(name || '').trim())} ${capitalize(slugify(surname || '').trim())}`;
}

function diffObjectsList(objects: Record<string, unknown>[]) {
  const diff: Record<string, unknown[]> = {};
  const keys = new Set(objects.flatMap((obj) => Object.keys(obj)));

  for (const key of keys) {
    if (['key', 'course_key', 'ref_key', 'comment'].includes(key)) continue;
    const values = new Set(objects.map((obj) => obj[key]));
    if (values.size > 1) {
      diff[key] = [...values];
    }
  }

  return diff;
}
