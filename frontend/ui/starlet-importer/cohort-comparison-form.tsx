import { useAtomValue } from 'jotai';
import { starletSettingsAtom } from './state';
import { useMutation, useQuery } from 'urql';
import {
  ArchiveCohortDocument,
  CohortListDocument,
  CreateCohortDocument,
  UpdateCohortDocument,
} from '@/graphql/Cohorts';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import React, { useMemo, type JSX } from 'react';
import Link from 'next/link';

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
      if (task[0] === 'create') {
        const [, name, description, id] = task;
        const hex = Math.floor(Math.random() * 0xff_ff_ff)
          .toString(16)
          .padStart(6, '0');
        await create({
          input: { name, description, colorRgb: `#${hex}`, externalIds: [id] },
        });
      } else if (task[0] === 'update') {
        await update({
          id: task[1],
          patch: { externalIds: [task[2]] },
        });
      } else {
        const [, id] = task;
        await archive({ id });
      }
    }
  });

  return (
    <div>
      <ul>{views}</ul>

      {tasks.length > 0 && (
        <SubmitButton
          className="mb-2"
          onClick={onSubmit.execute}
          loading={onSubmit.loading}
        >
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
  courses: [string, string, string][],
) {
  const views: JSX.Element[] = [];
  const tasks: ['create' | 'update' | 'archive', string, string, string][] = [];

  for (const course of courses) {
    const cohort = cohorts.find((cohort) => cohort.name === course[1]);
    if (cohort) {
      if (!cohort.externalIds?.includes(course[0])) {
        tasks.push(['update', cohort.id, course[0], '']);
      }
      views.push(
        <li key={course[0]} className="my-0">
          {course[1]} - {course[2]}
          <ul className="my-0">
            <li className="mt-0">
              ✅{' '}
              <Link
                href={{ pathname: '/treninkove-skupiny/[id]', query: { id: cohort.id } }}
              >
                {cohort.name}
              </Link>
              {!cohort.externalIds?.includes(course[0])
                ? ' - změnilo se ID kurzu, bude aktualizováno'
                : ''}
            </li>
          </ul>
        </li>,
      );
    } else {
      tasks.push(['create', course[1], course[2], course[0]]);
      views.push(
        <li key={course[0]} className="my-0">
          {course[1]} - {course[2]}
          <ul className="my-0">
            <li className="mt-0">❌ Tréninková skupina bude vytvořena</li>
          </ul>
        </li>,
      );
    }
  }
  for (const cohort of cohorts) {
    if (courses.some((course) => cohort.name === course[1])) continue;
    if (!cohort.isVisible) continue;
    tasks.push(['archive', cohort.id, '', '']);
    views.push(
      <li key={cohort.id} className="my-0">
        ❌
        <ul className="my-0">
          <li className="mt-0">
            <Link
              href={{ pathname: '/treninkove-skupiny/[id]', query: { id: cohort.id } }}
            >
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
