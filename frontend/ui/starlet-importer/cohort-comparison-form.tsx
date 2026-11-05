import { useAtomValue } from 'jotai';
import { type StarletCourse, starletSettingsAtom } from './state';
import { useMutation, useQuery } from 'urql';
import {
  ArchiveCohortDocument,
  CohortListDocument,
  CreateCohortDocument,
  UpdateCohortDocument,
} from '@/graphql/Cohorts';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import { type JSX, useMemo } from 'react';
import Link from 'next/link';

type CohortTask =
  | { type: 'create'; name: string; description: string; externalId: string }
  | { type: 'update'; id: string; externalId: string }
  | { type: 'archive'; id: string };

export function CohortComparisonForm() {
  const [{ data: cohortQuery }] = useQuery({
    query: CohortListDocument,
  });
  const cohorts = cohortQuery?.cohortsList;
  const { courses } = useAtomValue(starletSettingsAtom);

  const [, create] = useMutation(CreateCohortDocument);
  const [, update] = useMutation(UpdateCohortDocument);
  const [, archive] = useMutation(ArchiveCohortDocument);

  const [tasks, views] = useMemo(
    () => compare(cohorts || [], courses),
    [cohorts, courses],
  );

  const onSubmit = useAsyncCallback(async () => {
    for (const task of tasks) {
      if (task.type === 'create') {
        const { name, description, externalId } = task;
        const hex = Math.floor(Math.random() * 0xff_ff_ff)
          .toString(16)
          .padStart(6, '0');
        const result = await create({
          input: { name, description, colorRgb: `#${hex}`, externalIds: [externalId] },
        });
        if (result.error) throw result.error;
      } else if (task.type === 'update') {
        const result = await update({
          id: task.id,
          patch: { externalIds: [task.externalId] },
        });
        if (result.error) throw result.error;
      } else {
        const result = await archive({ id: task.id });
        if (result.error) throw result.error;
      }
    }
  });

  return (
    <div>
      <ul>{views}</ul>

      {tasks.length > 0 && (
        <SubmitButton className="mb-2" action={onSubmit}>
          Synchronizovat
        </SubmitButton>
      )}
    </div>
  );
}

function compare(
  cohorts: {
    id: string;
    name: string;
    isVisible: boolean;
    externalIds: (string | null)[] | null;
  }[],
  courses: StarletCourse[],
) {
  const views: JSX.Element[] = [];
  const tasks: CohortTask[] = [];

  for (const { id, code, name } of courses) {
    const cohort = cohorts.find((cohort) => cohort.name === code);
    const needsUpdate = !!cohort && !cohort.externalIds?.includes(id);
    if (!cohort) {
      tasks.push({ type: 'create', name: code, description: name, externalId: id });
    } else if (needsUpdate) {
      tasks.push({ type: 'update', id: cohort.id, externalId: id });
    }
    views.push(
      <li key={id} className="my-0">
        {code} - {name}
        <ul className="my-0">
          <li className="mt-0">
            {cohort ? <>
              ✅{' '}
              <Link href={`/treninkove-skupiny/${cohort.id}`}>
                {cohort.name}
              </Link>
              {needsUpdate
                ? ' - změnilo se ID kurzu, bude aktualizováno'
                : ''}
            </> : '❌ Tréninková skupina bude vytvořena'}
          </li>
        </ul>
      </li>,
    );
  }
  for (const cohort of cohorts) {
    if (courses.some((course) => cohort.name === course.code)) continue;
    if (!cohort.isVisible || !cohort.externalIds || cohort.externalIds.length === 0) continue;
    tasks.push({ type: 'archive', id: cohort.id });
    views.push(
      <li key={cohort.id} className="my-0">
        ❌
        <ul className="my-0">
          <li className="mt-0">
            <Link href={`/treninkove-skupiny/${cohort.id}`}>
              {cohort.name}
            </Link>{' '}
            Tréninková skupina bude archivována
          </li>
        </ul>
      </li>,
    );
  }

  return [tasks, views] as const;
}
