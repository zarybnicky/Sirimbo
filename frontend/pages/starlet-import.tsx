import { Layout } from '@/components/layout/Layout';
import { EvidenceStarletDocument, TenantSettingsDocument, UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import React, { useEffect, useMemo, useState } from 'react';
import { useQuery } from 'urql';
import { Course, Student, UserRole } from '@/starlet/graphql';
import { useZodForm } from '@/lib/use-schema-form';
import { TextFieldElement } from '@/ui/fields/text';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { type TypeOf, z } from 'zod';
import { fetchGql } from '@/graphql/query';
import { CourseDocument, EnumerateCoursesDocument, FoldersAndSeasonsDocument } from '@/starlet/graphql/Query';
import { print } from '@0no-co/graphql.web';
import { Checkbox } from '@/ui/fields/checkbox';
import { slugify } from '@/ui/slugify';
import { capitalize } from '@/ui/format';

type LoginToken =
  | { auth_ok: false }
  | {
    auth_ok: true;
    login: string;
    auth_token: string;
    role: UserRole;
    name: string;
    cgroup_key: string;
  };

export default function ProfilePage() {
  const tenantId = process.env.NEXT_PUBLIC_TENANT_ID || '1';
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    variables: { tenantId },
  });
  const update = useMutation(UpdateTenantSettingsDocument)[1];

  const settings = JSON.parse(settingsQuery?.tenantSetting?.settings || '{}');
  const loginDetails = useMemo(() => settings?.['evidenceAuth'], [JSON.stringify(settings)]);

  const [loginToken, setLoginToken] = useState<LoginToken | null>(null);
  useEffect(() => {
    if (loginToken?.auth_ok)
      return;
    if (!loginDetails?.login || !loginDetails?.password) {
      setLoginToken({ auth_ok: false });
      return;
    }

    fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/spa_auth/login',
      data: JSON.stringify({
        login: loginDetails.login,
        password: loginDetails.password,
      }),
    }).then(x => setLoginToken(JSON.parse(x.evidenceStarlet)));
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [loginDetails?.login, loginDetails?.password]);

  const [foldersAndSeasons, setFoldersAndSeasons] = useState<{
    data: {
      folders: {
        key: string;
        name: string;
        order_value: number;
      }[];
      seasons: {
        key: string;
        name: string;
        order_value: number;
      }[];
    }
  } | null>(null);

  useEffect(() => {
    if (!loginToken?.auth_ok) return

    fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/graphql',
      data: JSON.stringify({
        query: print(FoldersAndSeasonsDocument),
        variables: {},
      }),
      auth: loginToken.auth_token,
    }).then(x => setFoldersAndSeasons(JSON.parse(x.evidenceStarlet)));
  }, [loginToken]);

  const [selectedFolders, setSelectedFolders] = useState(new Set());
  const [selectedSeasons, setSelectedSeasons] = useState(new Set());
  useEffect(() => {
    setSelectedFolders(new Set(settings?.['evidenceFolders']));
    setSelectedSeasons(new Set(settings?.['evidenceSeasons']));
  }, [JSON.stringify(settings)]);

  const onSubmitFolders = useAsyncCallback(async () => {
    await update({
      input: {
        tenantId,
        patch: {
          settings: JSON.stringify({ ...settings, evidenceFolders: [...selectedFolders.values()], evidenceSeasons: [...selectedSeasons.values()] })
        }
      },
    });
  });

  const [courses, setCourses] = useState<{ key: string, name: string; period: string; code: string; evi_group: string; }[]>([]);
  useEffect(() => {
    if (!loginToken?.auth_ok) return;

    let promise = Promise.resolve();
    const result: { key: string, name: string; period: string; code: string; evi_group: string }[] = [];
    for (const folder of selectedFolders.values()) {
      for (const season of selectedSeasons.values()) {
        promise = promise.then(() => {
          return fetchGql(EvidenceStarletDocument, {
            url: 'https://evidence.tsstarlet.com/graphql',
            data: JSON.stringify({
              query: print(EnumerateCoursesDocument),
              variables: { folder, season },
            }),
            auth: loginToken.auth_token,
          }).then(x => {
            result.push(...JSON.parse(x.evidenceStarlet).data.courses);
          });
        })
      }
    }
    promise.then(() => setCourses(result));
  }, [loginToken, selectedFolders, selectedSeasons]);

  const [fullCourses, setFullCourses] = useState<{ course: Course, students: Student[] }[]>([]);
  const [students, setStudents] = useState<Map<string, (Student & { partner: string; })[]>>(new Map());

  useEffect(() => {
    if (!loginToken?.auth_ok) return;

    let promise = Promise.resolve();
    const result: { course: Course, students: Student[] }[] = [];
    const studentMap = new Map<string, (Student & { partner: string; })[]>();
    for (const course of courses) {
      promise = promise.then(() => {
        return fetchGql(EvidenceStarletDocument, {
          url: 'https://evidence.tsstarlet.com/graphql',
          data: JSON.stringify({
            query: print(CourseDocument),
            variables: { key: course.key },
          }),
          auth: loginToken.auth_token,
        }).then(x => {
          const data: { course: Course, students: Student[] } = JSON.parse(x.evidenceStarlet).data;
          result.push(data);
          for (const student of data.students) {
            const name = `${capitalize(slugify(student.name || ''))} ${capitalize(slugify(student.surname || ''))}`;
            if (!studentMap.has(name))
              studentMap.set(name, []);
            const partner = student.partner_ref_key ? data.students.find(s => s.ref_key === student.partner_ref_key) : null;
            studentMap.get(name)!.push({
              ...student,
              email: (student.email || '').toLowerCase(),
              phone: (student.phone || '').replaceAll(' ', '').replace(/^\+420/, ''),
              post_code: (student.post_code || '').replaceAll(' ', ''),
              partner: partner ? `${partner.name} ${partner.surname}` : '',
            });
          }
        });
      })
    }
    promise.then(() => setFullCourses(result));
    promise.then(() => setStudents(studentMap));
  }, [courses, loginToken]);

  return (
    <Layout requireAdmin>
      <TitleBar title="Můj profil" />

      <div className="prose">
        <h2>1. Přihlašovací údaje</h2>

        <p>
          Přihlašovací údaje {loginDetails ? 'vyplněny' : 'nevyplněny'}
          <Dialog>
            <DialogTrigger size="sm" text="Změnit přihlašovací údaje" />
            <DialogContent>
              <ChangeLoginForm tenantId={tenantId} settings={settings} />
            </DialogContent>
          </Dialog>
        </p>

        <p>
          {loginToken ?
            (loginToken.auth_ok ?
              `Přihlášen jako ${loginToken.login}` :
              'Neplatné přihlašovací údaje') :
            'Pokouším se přihlásit...'}
        </p>

        <h2 className="mb-0">2. Sezóny a složky</h2>
        <div className="grid grid-cols-2">
          {foldersAndSeasons ? (
            <>
              <ul>
                {foldersAndSeasons.data.folders
                  .sort((x, y) => x.order_value - y.order_value)
                  .map(x => (
                    <li key={x.key}>
                    <Checkbox name={x.key} label={x.name} value={x.key} checked={selectedFolders.has(x.key)} onChange={(e) => setSelectedFolders(fs => (e.target as any).checked ? fs.union(new Set([x.key])) : fs.difference(new Set([x.key])))} />
                    </li>
                  ))
                }
              </ul>
              <ul>
                {foldersAndSeasons.data.seasons
                  .sort((x, y) => x.order_value - y.order_value)
                  .map(x => (
                    <li key={x.key}>
                      <Checkbox name={x.key} label={x.name} value={x.key} checked={selectedSeasons.has(x.key)} onChange={(e) => setSelectedSeasons(fs => (e.target as any).checked ? fs.union(new Set([x.key])) : fs.difference(new Set([x.key])))} />
                    </li>
                  ))
                }
              </ul>
            </>
          ) : null}
        </div>

        <SubmitButton type="button" loading={onSubmitFolders.loading} onClick={onSubmitFolders.execute} />

        <h2>3. Kurzy</h2>

        <ul>
          {courses.map(x => <li key={x.key}>{x.code} {x.name} {x.period ? `(${x.period})` : ''}</li>)}
        </ul>

        <h2>4. Studenti</h2>

        {/*fullCourses.map(course => (
          <>
            <h3>{course.course.code}</h3>
            <ul>
              {course.students.map(student => (
                <li key={student.key}>{student.name} {student.surname}</li>
              ))}
            </ul>
          </>
        ))*/}

        <h2>Duplikovaní podle jména</h2>

        <ul>
        {[...students.entries()].filter(x => x[1].length > 1).map(x => (
          <li key={x[0]}>
            {x[0]}
            {' - objeven v '}
            {x[1]
              .map(y => y.course_key)
              .map(key => fullCourses.find(course => course.course.key === key)?.course.code)
              .join(', ')}
            <ul>
              {Object.entries(diffObjectsList(x[1] as any as Record<string, string>[])).map(x => <li key={x[0]}>{x[0]}: {JSON.stringify(x[1])}</li>)}
            </ul>
          </li>
        ))}
        </ul>

        K vytvoření

        Shodní

        Provést import
      </div>
    </Layout>
  );
};

function diffObjectsList(objects: Record<string, string>[]) {
  const diff: Record<string, string[]> = {};
  const keys = new Set(objects.flatMap(obj => Object.keys(obj)));

  for (const key of keys) {
    if (['key', 'card_id', 'short_id', 'course_key', 'ref_gid', 'ref_key', 'var_sym', 'reg_datetime', 'course_cost', 'paid_amount', 'reg_online', 'reg_by_admin', 'card_out', 'comment', 'discount'].includes(key))
      continue;
    const values = new Set(objects.map(obj => obj[key]!));
    if (values.size > 1) {
      diff[key] = [...values];
    }
  }

  return diff;
}

const Form = z.object({
  login: z.string(),
  password: z.string(),
});

export function ChangeLoginForm({ tenantId, settings }: {
  tenantId: string;
  settings: Record<string, object>;
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useZodForm(Form);
  const update = useMutation(UpdateTenantSettingsDocument)[1];

  useEffect(() => reset(settings['evidenceAuth']), []);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({
      input: {
        tenantId,
        patch: {
          settings: JSON.stringify({ ...settings, evidenceAuth: values })
        }
      },
    });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="login" label="Přihlašovací jméno" />

      <TextFieldElement control={control} name="password" label="Heslo" />

      Otestovat přihlášení

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
