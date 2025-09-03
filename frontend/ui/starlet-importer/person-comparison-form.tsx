import { EvidenceStarletDocument } from '@/graphql/CurrentUser';
import { fetchGql } from '@/graphql/query';
import { Course, Student } from '@/starlet/graphql';
import { CourseDocument, CourseQuery } from '@/starlet/graphql/Query';
import { capitalize } from '@/ui/format';
import { slugify } from '@/ui/slugify';
import { print } from '@0no-co/graphql.web';
import React, { useEffect, useMemo, useState } from 'react';
import { CreatePersonDocument, PersonListDocument, PersonMembershipsDocument, UpdatePersonDocument } from '@/graphql/Person';
import { useMutation, useQuery } from 'urql';
import { SubmitButton } from '../submit';
import { useAtomValue } from 'jotai';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { truthyFilter } from '../truthyFilter';
import { CohortListDocument, SyncCohortMembershipsDocument } from '@/graphql/Cohorts';
import Link from 'next/link';
import { useAsyncCallback } from 'react-async-hook';
import { tenantId } from '@/tenant/config';
import { UpdateTenantMembershipDocument } from '@/graphql/Memberships';

type QueriedStudent = Pick<
  Student,
  | 'key'
  | 'course_key'
  | 'sex'
  | 'ref_key'
  | 'partner_ref_key'
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
  partner_name: string | null;
  course_name: string;
};
type CleanedCourse = { course: Course; students: QueriedStudent[] };

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
  partner_ref_keys: string[];
  partner_names: string[];
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
    postalCode: string | null
    orientationNumber: string | null;
    conscriptionNumber: string | null;
  } | null;
  externalIds: (string | null)[] | null;
  isTrainer: boolean | null;
  isAdmin: boolean | null;
};

export function PersonComparisonForm() {
  const token = useAtomValue(starletTokenAtom);
  const { courses } = useAtomValue(starletSettingsAtom);
  const [{ data: personQuery }] = useQuery({ query: PersonListDocument });
  const [{ data: cohortQuery }] = useQuery({ query: CohortListDocument });
  const persons = personQuery?.filteredPeopleList || [];
  const cohorts = cohortQuery?.getCurrentTenant?.cohortsList || [];

  const [coursesWithStudents, setCoursesWithStudents] = useState<CleanedCourse[]>([]);
  useEffect(() => {
    if (!token?.auth_ok) return;
    fetchCoursesWithStudents(token.auth_token, courses.map((x) => x[0])).then(setCoursesWithStudents);
  }, [token?.auth_ok, token?.auth_ok ? token.auth_token : '', courses]);

  const [students, problematic] = useMemo(() => deduplicateStudents(coursesWithStudents), [coursesWithStudents]);
  const [tasks, views] = useMemo(() => compare(persons, students, cohorts), [persons, students, cohorts]);

  const create = useMutation(CreatePersonDocument)[1];
  const update = useMutation(UpdatePersonDocument)[1];
  const updateMembership = useMutation(UpdateTenantMembershipDocument)[1];
  const syncCohorts = useMutation(SyncCohortMembershipsDocument)[1];

  const onSubmit = useAsyncCallback(async () => {
    for (const [task, student, person, cohortIds] of tasks) {
      if (task === 'create' && student) {
        const res = await create({
          input: {
            p: {
              firstName: student.name,
              lastName: student.surname,
              gender: student.gender,
              nationality: '203',
              email: student.email,
              phone: student.phone,
              birthDate: student.year ? `${student.year}-01-01` : null,
              externalIds: student.ref_keys,
              address: {
                street: student.street ?? '',
                orientationNumber: student.street_no ?? '',
                city: student.city ?? '',
                postalCode: student.post_code ?? '',
                conscriptionNumber: '',
                district: '',
                region: '',
              },
              prefixTitle: '',
              suffixTitle: '',
              bio: '',
            },
            isMember: true,
            joinDate: new Date().toISOString(),
          },
        });
        if (res.data?.createPerson?.p?.id) {
          await syncCohorts({
            input: {
              personId: res.data.createPerson.p.id,
              cohortIds,
            },
          });
        }
      } else if (task === 'update' && student) {
        await update({
          input: {
            id: person!.id,
            patch: {
              firstName: student.name,
              lastName: student.surname,
              gender: student.gender,
              nationality: '203',
              email: student.email,
              phone: student.phone,
              birthDate: student.year ? `${student.year}-01-01` : null,
              externalIds: student.ref_keys,
              address: {
                street: student.street ?? '',
                orientationNumber: student.street_no ?? '',
                city: student.city ?? '',
                postalCode: student.post_code ?? '',
                conscriptionNumber: '',
                district: '',
                region: '',
              },
              prefixTitle: '',
              suffixTitle: '',
              bio: '',
            },
          },
        });
        await syncCohorts({
          input: {
            personId: person!.id,
            cohortIds,
          },
        });
      } else {
        await update({
          input: {
            id: person!.id,
            patch: {
              externalIds: [],
            },
          }
        });
        await syncCohorts({
          input: {
            personId: person!.id,
            cohortIds: [],
          },
        });
        const memberships = await fetchGql(PersonMembershipsDocument, { id: person!.id });
        const currentMembership = memberships.person?.tenantMembershipsList.find(x => x.tenant?.id === tenantId);
        if (currentMembership) {
          await updateMembership({
            input: {
              id: currentMembership.id,
              patch: {
                status: 'EXPIRED',
                until: new Date().toISOString(),
              }
            }
          });
        }
      }
    }
  });

  if (coursesWithStudents.length === 0)
    return null;

  return (
    <>
      {/*coursesWithStudents
        .map(course => [course, detectCouples(course)] as const)
        .map(([course, [couples, solos]]) => (
        <React.Fragment key={course.course.key}>
          <h3>{course.course.code}</h3>
          <p>
            {'Páry: '}
            {[...couples].join(', ')}
          </p>
          <p>
            {'Sólo: '}
            {[...solos].join(', ')}
          </p>
        </React.Fragment>
        ))*/}

      <h3>Problémy při sjednocení přihlášek</h3>
      <ul>
        {problematic.map(([name, candidates]) => (
          <li key={name}>
            {name}
            {candidates.length > 0 && (
              <>
                {' - objeven v '}
                {candidates.map(x => x.course_name).join(', ')}
                <ul>
                  {Object.entries(
                    diffObjectsList(candidates as any as Record<string, string>[]),
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
        {(problematic.length == 0 || problematic.every(x => x[1].length === 0)) && (
          <li>✅ Všechny v pořádku</li>
        )}
      </ul>

      <h3>Osoby bez údajů</h3>
      <ul>
        {students.filter(x => !x.email).map(x => <li>{x.normal_name} ({x.course_names.join(', ')}): chybí e-mail</li>)}
        {students.filter(x => !x.phone).map(x => <li>{x.normal_name} ({x.course_names.join(', ')}): chybí telefon</li>)}
        {students.filter(x => !x.year).map(x => <li>{x.normal_name} ({x.course_names.join(', ')}): chybí rok narození</li>)}
      </ul>

      <h3>Výsledný změny k synchronizaci</h3>
      <ul>
        {views}
        {views.length === 0 && (
          <li>✅ Žádné úpravy nejsou potřeba</li>
        )}
      </ul>

      {tasks.length > 0 && (
        <SubmitButton
          className="mb-2"
          onClick={onSubmit.execute}
          loading={onSubmit.loading}
        >
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
  }[],
) {
  const views: JSX.Element[] = [];
  const tasks: ['create' | 'update' | 'archive', DeduplicatedStudent | null, Person | null, string[]][] = [];

  const personMap = new Map<string, Person[]>();
  for (const person of people) {
    const normalName = getNormalizedName(person.firstName, person.lastName);
    const people = personMap.get(normalName) || [];
    people.push(person);
    personMap.set(normalName, people);
  }

  const processedPeople = new Set<string>();
  for (const student of students) {
    const candidates = (personMap.get(student.normal_name) || []).filter(x => !processedPeople.has(x.id));

    const person = candidates.length > 0 ? disambiguateCandidates(student, candidates) : undefined;

    if (!person) {
      const cohortIds = cohorts.filter(x => student.course_names.includes(x.name)).map(x => x.id);
      tasks.push(['create', student, null, cohortIds]);
      // include cohort memberships, map course_names to cohortIds

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

    const birthYear = person.birthDate ? new Date(person.birthDate).getFullYear() : null;
    const courseList = new Set(student.course_names);
    const cohortList = new Set(cohorts.filter(x => (person.cohortIds || []).includes(x.id)).map(x => x.name));

    let willUpdate = false;
    if (new Set(person.externalIds || []).symmetricDifference(new Set(student.ref_keys)).size > 0 || person.email !== student.email || person.phone !== student.phone || person.gender !== student.gender || birthYear !== student.year || cohortList.symmetricDifference(courseList).size > 0) {
      willUpdate = true;
      const cohortIds = cohorts.filter(x => student.course_names.includes(x.name)).map(x => x.id);
      tasks.push(['update', student, person, cohortIds]);
    }
    processedPeople.add(person.id);
    if (willUpdate) {
      views.push(
        <li key={student.ref_keys.join(',')} className="my-0">
          {student.normal_name} ({student.year})
          <ul className="my-0">
            <li className="mt-0">
              ✅{' '}
              <Link href={{ pathname: '/clenove/[id]', query: { id: person.id } }}>
                {person.name} ({birthYear})
              </Link>
              {willUpdate ? ' - změnily se detaily osoby, bude aktualizovaná' : ''}
            </li>
          </ul>
        </li>,
      );
    }
  }
  for (const person of people) {
    if (processedPeople.has(person.id) || person.isAdmin || person.isTrainer)
      continue;
    const birthYear = new Date(person.birthDate || '1900-01-01').getFullYear();

    tasks.push(['archive', null, person, []]);
    views.push(
      <li key={person.id} className="my-0">
        ❌
        <ul className="my-0">
          <li className="mt-0">
            <Link href={{ pathname: '/clenove/[id]', query: { id: person.id } }}>
              {person.name} ({birthYear})
            </Link>{' '}
            Osoba bude archivována
          </li>
        </ul>
      </li>,
    );
  }
  return [tasks, views] as const;
}

function disambiguateCandidates(student: DeduplicatedStudent, candidates: Person[]) {
  const byBirthYears = candidates.map(person => [new Date(person.birthDate || '1900-01-01').getFullYear(), person] as const);
  return [
    student.year ? byBirthYears.find(x => x[0] === student.year) : undefined,
    byBirthYears.find(x => x[0] === 1900),
    !student.year ? byBirthYears.find(Boolean) : undefined,
  ].filter(truthyFilter).map(x => x[1]).find(Boolean);
}

async function fetchCoursesWithStudents(auth_token: string, courses: string[]) {
  const result: CleanedCourse[] = [];

  for (const course of courses) {
    const { evidenceStarlet } = await fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/graphql',
      data: JSON.stringify({
        query: print(CourseDocument),
        variables: { key: course },
      }),
      auth: auth_token,
    });

    const data = JSON.parse(evidenceStarlet).data as CourseQuery;
    if (!data?.course) continue;

    const students = data.students || [];
    const cleanedStudents = [];
    for (const student of students) {
      if (!student) continue;

      const name = getNormalizedName(student.name, student.surname);
      const partner = student.partner_ref_key
        ? students.find((s) => s?.ref_key === student.partner_ref_key)
        : null;

      const cleanedStudent: QueriedStudent = {
        normal_name: name,
        partner_name: partner ? getNormalizedName(partner.name, partner.surname) : null,
        course_name: data.course.code!,
        key: student.key,
        course_key: student.course_key,
        sex: student.sex,
        ref_key: student.ref_key,
        partner_ref_key: student.partner_ref_key,
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
    result.push({ course: data.course as any, students: cleanedStudents });
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
    if (candidates.length === 0)
      continue;

    const birthYears = new Set(candidates.map(x => x.year).filter(truthyFilter));
    const phones = new Set(candidates.map(x => x.phone).filter(truthyFilter));
    const emails = new Set(candidates.map(x => x.email).filter(truthyFilter));

    if (birthYears.size < 2) {
      if (phones.size < 2 && emails.size < 2) {
        // clean merge or a single candidate, pick first in case of address conflict
        deduplicated.push(mergeCandidates(candidates));
        if (candidates.length > 1)
          problematic.push([`${normalName}: více přihlášek studenta sloučeno`, []]);
      } else {
        problematic.push(['Stejný rok narození ale jiné kontaktní údaje, nemůžu sloučit', candidates]);
        continue;
      }
    } else if (birthYears.size >= phones.size && birthYears.size >= emails.size) {
      if (candidates.map(x => x.year).some(x => !x)) {
        // some birth year is falsy, bail
        problematic.push(['Podle roku narození jiní lidé, ale některým rok narození chybí, nemůžu rozdělit', candidates]);
        continue;
      }
      for (const discriminant of birthYears) {
        deduplicated.push(mergeCandidates(candidates.filter(x => x.year === discriminant)));
      }
      if (birthYears.size > 1)
        problematic.push([`${normalName}: konflikt vyřešen rozdělením podle roků narození (${[...birthYears].join(', ')})`, []]);
    }
  }

  deduplicated.sort((x, y) => `${x.surname}, ${x.name}`.localeCompare(`${y.surname}, ${y.name}`));

  return [deduplicated, problematic] as const;
}

function mergeCandidates(candidates: QueriedStudent[]): DeduplicatedStudent {
  const sex = candidates.map(x => x.sex).find(truthyFilter);
  return {
    normal_name: candidates.map(x => x.normal_name).find(truthyFilter)!,
    name: candidates.map(x => x.name).find(truthyFilter) || '?',
    surname: candidates.map(x => x.surname).find(truthyFilter) || '?',
    gender: sex === 'FEMALE' ? 'WOMAN' : 'MAN',
    email: candidates.map(x => x.email).find(truthyFilter) || null,
    phone: candidates.map(x => x.phone).find(truthyFilter) || null,
    year: candidates.map(x => x.year).find(truthyFilter) || null,
    street: candidates.map(x => x.street).find(truthyFilter) || null,
    street_no: candidates.map(x => x.street_no).find(truthyFilter) || null,
    city: candidates.map(x => x.city).find(truthyFilter) || null,
    post_code: candidates.map(x => x.post_code).find(truthyFilter) || null,
    keys: candidates.map(x => x.key).filter(truthyFilter),
    course_names: candidates.map(x => x.course_name).filter(truthyFilter),
    course_keys: candidates.map(x => x.course_key).filter(truthyFilter),
    ref_keys: candidates.map(x => x.ref_key).filter(truthyFilter),
    partner_names: candidates.map(x => x.partner_name).filter(truthyFilter),
    partner_ref_keys: candidates.map(x => x.partner_ref_key).filter(truthyFilter),
  };
}

// needs deduplicated students
function detectCouples(course: CleanedCourse) {
  const couples = new Set<string>();
  const solos = new Set<string>();

  for (const { partner_name, normal_name, sex } of course.students) {
    if (partner_name) {
      couples.add(
        sex === 'MALE'
          ? `${normal_name} - ${partner_name}`
          : `${partner_name} - ${normal_name}`,
      );
    } else {
      solos.add(normal_name);
    }
  }

  return [couples, solos] as const;
}

function getNormalizedName(
  name: string | null,
  surname: string | null,
): string {
  return `${capitalize(slugify(name || '').trim())} ${capitalize(slugify(surname || '').trim())}`;
}

function diffObjectsList(objects: Record<string, string>[]) {
  const diff: Record<string, string[]> = {};
  const keys = new Set(objects.flatMap((obj) => Object.keys(obj)));

  for (const key of keys) {
    if (['key', 'course_key', 'ref_key', 'comment'].includes(key)) continue;
    const values = new Set(objects.map((obj) => obj[key]!));
    if (values.size > 1) {
      diff[key] = [...values];
    }
  }

  return diff;
}
