import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { EnumerateCoursesDocument } from '@/starlet/graphql/Query';
import { VerticalCheckboxButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useEffect, useState } from 'react';
import { useMutation } from 'urql';
import { z } from 'zod';
import { starletSettingsAtom, starletTokenAtom } from './state';
import { useAtomValue } from 'jotai';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { fetchStarlet } from '@/starlet/query';

const Form = z.object({
  courses: z.array(z.string()),
});

type SimpleCourse = {
  key: string;
  name: string;
  period: string | null;
  code: string;
};

export function ChangeCoursesForm() {
  const { onSuccess } = useFormResult();
  const { folders, seasons, courses: prevCourses } = useAtomValue(starletSettingsAtom);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { courses: prevCourses.map(([id]) => id) },
  });
  const [result, update] = useMutation(UpdateTenantSettingsDocument);
  const token = useAtomValue(starletTokenAtom);

  const [courses, setCourses] = useState<SimpleCourse[]>([]);

  useEffect(() => {
    if (!token?.auth_ok) return;

    fetchCoursesByFolders(
      folders.map((x) => x[0]),
      seasons.map((x) => x[0]),
    ).then(setCourses);
  }, [folders, token?.auth_ok, seasons]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const selectedCourses = values.courses.map((id) => {
      const course = courses.find((course) => course.key === id);
      const code = course ? course.code : '?';
      const name = course
        ? `${course.name}${course.period ? ` (${course.period})` : ''}`
        : '?';
      return [id, code, name];
    });
    const result = await update({
      input: {
        path: ['evidenceCourses'],
        newValue: selectedCourses,
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <VerticalCheckboxButtonGroupElement
        control={control}
        name="courses"
        options={courses.map((course) => ({
          id: course.key,
          label: `${course.code} ${course.name}${course.period ? ` (${course.period})` : ''}`,
        }))}
      />
      <SubmitButton control={control} />
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
