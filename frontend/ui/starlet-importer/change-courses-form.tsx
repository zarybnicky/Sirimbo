import {
  EvidenceStarletDocument,
  UpdateTenantSettingsDocument,
} from '@/graphql/CurrentUser';
import { fetchGql } from '@/graphql/query';
import { EnumerateCoursesDocument } from '@/starlet/graphql/Query';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { print } from '@0no-co/graphql.web';
import React, { useEffect, useState } from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  courses: z.record(z.string(), z.boolean().prefault(false)),
});

type SimpleCourse = {
  key: string;
  name: string;
  period: string;
  code: string;
  evi_group: string;
};

export function ChangeCoursesForm() {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useForm<z.infer<typeof Form>>({
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
      token.auth_token,
      folders.map((x) => x[0]),
      seasons.map((x) => x[0]),
    ).then(setCourses);
  }, [folders, token, seasons]);

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

async function fetchCoursesByFolders(
  token: string,
  folders: string[],
  seasons: string[],
) {
  const result: SimpleCourse[] = [];

  for (const folder of folders) {
    for (const season of seasons) {
      const { evidenceStarlet } = await fetchGql(EvidenceStarletDocument, {
        url: 'https://evidence.tsstarlet.com/graphql',
        data: JSON.stringify({
          query: print(EnumerateCoursesDocument),
          variables: { folder, season },
        }),
        auth: token,
      });
      result.push(...JSON.parse(evidenceStarlet).data.courses);
    }
  }
  return result.sort((x, y) => x.code.localeCompare(y.code));
}
