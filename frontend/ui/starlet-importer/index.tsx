import { EvidenceStarletDocument } from '@/graphql/CurrentUser';
import { fetchGql } from '@/graphql/query';
import { Course, Student } from '@/starlet/graphql';
import { CourseDocument, CourseQuery } from '@/starlet/graphql/Query';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { capitalize } from '@/ui/format';
import { slugify } from '@/ui/slugify';
import { print } from '@0no-co/graphql.web';
import React, { useEffect, useState } from 'react';
import { ChangeCoursesForm } from './change-courses-form';
import { ChangeFoldersForm } from './change-folders-form';
import { ChangeLoginForm } from './change-login-form';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { CohortComparisonForm } from './cohort-comparison-form';
import { SubmitButton } from '../submit';

export function StarletImporter() {
  const token = useAtomValue(starletTokenAtom);
  const { auth, folders, seasons, courses } = useAtomValue(starletSettingsAtom);

  const [coursesWithStudents, setCoursesWithStudents] = useState<
    { course: Course; couples: string[]; solos: string[]; students: Student[] }[]
  >([]);
  const [studentMap, setStudentMap] = useState<
    Map<string, (Student & { partner: string | null })[]>
  >(new Map());

  useEffect(() => {
    if (!token?.auth_ok) return;

    fetchCoursesWithStudents(
      token.auth_token,
      courses.map((x) => x[0]),
    ).then(([courses, students]) => {
      setCoursesWithStudents(courses);
      setStudentMap(students);
    });
  }, [courses, auth]);

  return (
    <div className="prose">
      <h2 className="flex justify-between my-0">
        <span>1. Přihlašovací údaje</span>
        <Dialog>
          <DialogTrigger size="sm" text="Nahradit přihlašovací údaje" />
          <DialogContent>
            <ChangeLoginForm />
          </DialogContent>
        </Dialog>
      </h2>

      <p>
        {auth ? (
          token ? (
            // eslint-disable-next-line unicorn/no-nested-ternary
            token.auth_ok ? (
              <>
                Přihlášen v evidenci jako <b>{token.login}</b>
              </>
            ) : (
              'Neplatné přihlašovací údaje do evidence'
            )
          ) : (
            'Pokouším se přihlásit do evidence...'
          )
        ) : (
          'Přihlašovací údaje do evidence nevyplněny'
        )}
      </p>

      <h2 className="flex justify-between my-0">
        <span>2. Sezóny a složky</span>
        {token?.auth_ok && (
          <Dialog>
            <DialogTrigger size="sm" text="Upravit výběr" />
            <DialogContent>
              <ChangeFoldersForm />
            </DialogContent>
          </Dialog>
        )}
      </h2>

      <p>
        Sezóny: {seasons.map((x) => x[1]).join(', ')}
        <br />
        Složky: {folders.map((x) => x[1]).join(', ')}
      </p>

      <h2 className="flex justify-between my-0">
        <span>3. Kurzy</span>
        {token?.auth_ok && (
          <Dialog>
            <DialogTrigger size="sm" text="Upravit výběr" />
            <DialogContent>
              <ChangeCoursesForm />
            </DialogContent>
          </Dialog>
        )}
      </h2>

      <CohortComparisonForm />

      <h2 className="flex justify-between my-0">
        <span>4. Studenti</span>
      </h2>

      {coursesWithStudents.map((course) => (
        <React.Fragment key={course.course.key}>
          <h3>{course.course.code}</h3>
          <p>
            {'Páry: '}
            {course.couples.join(', ')}
          </p>
          <p>
            {'Sólo: '}
            {course.solos.join(', ')}
          </p>
        </React.Fragment>
      ))}

      <h3>Duplikovaní podle jména</h3>

      <ul>
        {[...studentMap.entries()]
          .filter((x) => x[1].length > 1)
          .map((x) => (
            <li key={x[0]}>
              {x[0]}
              {' - objeven v '}
              {x[1]
                .map(
                  (student) =>
                    coursesWithStudents.find(
                      (course) => course.course.key === student.course_key,
                    )?.course.code,
                )
                .join(', ')}
              <ul>
                {Object.entries(
                  diffObjectsList(x[1] as any as Record<string, string>[]),
                ).map((x) => (
                  <li key={x[0]}>
                    {x[0]}: {JSON.stringify(x[1])}
                  </li>
                ))}
              </ul>
            </li>
          ))}
      </ul>

      <h3>Shodní</h3>

      <h3>K vytvoření</h3>

      <SubmitButton type="button">Provést import</SubmitButton>
    </div>
  );
}

function diffObjectsList(objects: Record<string, string>[]) {
  const diff: Record<string, string[]> = {};
  const keys = new Set(objects.flatMap((obj) => Object.keys(obj)));

  for (const key of keys) {
    if (
      [
        'key',
        'card_id',
        'short_id',
        'course_key',
        'ref_gid',
        'ref_key',
        'var_sym',
        'reg_datetime',
        'course_cost',
        'paid_amount',
        'reg_online',
        'reg_by_admin',
        'card_out',
        'discount',
        'comment',
      ].includes(key)
    )
      continue;
    const values = new Set(objects.map((obj) => obj[key]!));
    if (values.size > 1) {
      diff[key] = [...values];
    }
  }

  return diff;
}

async function fetchCoursesWithStudents(auth_token: string, courses: string[]) {
  const result: {
    course: Course;
    solos: string[];
    couples: string[];
    students: Student[];
  }[] = [];
  const studentMap = new Map<string, (Student & { partner: string | null })[]>();

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
    if (!data.course) continue;

    const couples = new Set<string>();
    const solos = new Set<string>();
    for (const student of data?.students || []) {
      if (!student) continue;
      if ([null, 'FILED', 'KICKED', 'KICKED2', 'SPARE'].includes(student.status))
        continue;

      const name = getNormalizedName(student);
      const existingStudents =
        studentMap.get(name) || studentMap.get(`${name} ${student.year}`) || [];
      const partner = student.partner_ref_key
        ? data.students?.find((s) => s?.ref_key === student.partner_ref_key)
        : null;
      existingStudents.push({
        ...student,
        email: (student.email || '').toLowerCase() || null,
        phone: (student.phone || '').replaceAll(' ', '').replace(/^\+420/, '') || null,
        post_code: (student.post_code || '').replaceAll(' ', '') || null,
        partner: partner ? getNormalizedName(partner) : null,
      } as any);
      if (partner) {
        const partnerName = getNormalizedName(partner);
        couples.add(
          student.sex === 'MALE'
            ? `${name} - ${partnerName}`
            : `${partnerName} - ${name}`,
        );
      } else {
        solos.add(name);
      }
      studentMap.set(name, existingStudents);
    }
    result.push({
      ...data,
      solos: [...solos.values()],
      couples: [...couples.values()],
    } as any);
  }
  return [result, studentMap] as const;
}

function getNormalizedName(student: {
  name: string | null;
  surname: string | null;
}): string {
  return `${capitalize(slugify(student.name || ''))} ${capitalize(slugify(student.surname || ''))}`;
}
