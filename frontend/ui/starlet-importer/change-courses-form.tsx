import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { EnumerateCoursesDocument } from '@/starlet/graphql/Query';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useEffect, useState } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { fetchStarlet } from '@/starlet/query';

const Form = z.object({
  courses: z.record(z.string(), z.boolean().prefault(false)),
});

type SimpleCourse = {
  key: string;
  name: string;
  period: string | null;
  code: string;
};

export function ChangeCoursesForm() {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useForm({
    resolver: zodResolver(Form),
  });
  const update = useMutation(UpdateTenantSettingsDocument)[1];
  const token = useAtomValue(starletTokenAtom);

  const { folders, seasons, courses: prevCourses } = useAtomValue(starletSettingsAtom);
  useEffect(() => {
    reset({
      courses: Object.fromEntries(prevCourses.map((x) => [x[0], true] as const)),
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const [courses, setCourses] = useState<SimpleCourse[]>([]);

  useEffect(() => {
    if (!token?.auth_ok) return;

    fetchCoursesByFolders(
      folders.map((x) => x[0]),
      seasons.map((x) => x[0]),
    ).then(setCourses);
  }, [folders, token?.auth_ok, seasons]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await update({
      input: {
        path: ['evidenceCourses'],
        newValue: JSON.stringify(
          Object.entries(values.courses)
            .filter((x) => x[1])
            .map((x) => {
              const c = courses.find((y) => y.key === x[0]);
              const code = c ? c.code : '?';
              const name = c ? `${c.name}${c.period ? ` (${c.period})` : ''}` : '?';
              return [x[0], code, name];
            }),
        ),
      },
    });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ul>
        {courses.map((x) => (
          <li key={x.key}>
            <CheckboxElement
              control={control}
              name={`courses.${x.key}`}
              label={`${x.code} ${x.name}${x.period ? ` (${x.period})` : ''}`}
            />
          </li>
        ))}
      </ul>
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}

async function fetchCoursesByFolders(folders: string[], seasons: string[]) {
  const result: SimpleCourse[] = [];

  for (const folder of folders) {
    for (const season of seasons) {
      const data = await fetchStarlet(EnumerateCoursesDocument, { folder, season });
      for (const course of data?.courses ?? []) {
        if (!course) continue;
        const { key, code, name, period } = course;
        if (!key || !code || !name) continue;
        result.push({ key, code, name, period });
      }
    }
  }
  return result.toSorted((x, y) => x.code.localeCompare(y.code));
}
