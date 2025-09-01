import { useAtomValue } from 'jotai';
import { starletSettingsAtom } from './state';
import { useQuery } from 'urql';
import { CohortListDocument } from '@/graphql/Cohorts';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import React from 'react';
import Link from 'next/link';

export function CohortComparisonForm() {
  const [{ data: cohortQuery }] = useQuery({
    query: CohortListDocument,
  });
  const cohorts = cohortQuery?.getCurrentTenant?.cohortsList || [];

  const { courses } = useAtomValue(starletSettingsAtom);

  const views: JSX.Element[] = [];
  const tasks: ['create' | 'archive', string, string][] = [];

  for (const course of courses) {
    const cohort = cohorts.find(cohort => cohort.name === course[1])
    if (cohort) {
      views.push(
        <li key={course[0]} className="my-0">
          {course[1]} - {course[2]}
          <ul className="my-0">
            <li className="mt-0">
              ✅
              {' '}
              <Link href={{ pathname: "/treninkove-skupiny/[id]", query: { id: cohort.id }}}>
                {cohort.name}
              </Link>
            </li>
          </ul>
        </li>
      );
    } else {
      tasks.push(['create', course[1], course[2]]);
      views.push(
        <li key={course[0]} className="my-0">
          {course[1]} - {course[2]}
          <ul className="my-0">
            <li className="mt-0">
              ❌ Tréninková skupina bude vytvořena
            </li>
          </ul>
        </li>
      );
    }
  }
  for (const cohort of cohorts) {
    if (courses.some(course => cohort.name === course[1]))
      continue;
    tasks.push(['archive', cohort.id, '']);
    views.push(
      <li key={cohort.id} className="my-0">
        ❌
        <ul className="my-0">
          <li className="mt-0">
            <Link href={{ pathname: "/treninkove-skupiny/[id]", query: { id: cohort.id }}}>
              {cohort.name}
            </Link>
            {' '}
            Tréninková skupina bude archivována
          </li>
        </ul>
      </li>
    );
  }

  const onSubmit = useAsyncCallback(async () => {

  });

  return (
    <div>
      <ul>
        {views}
      </ul>

      {tasks.length > 0 && (
        <SubmitButton className="mb-2" loading={onSubmit.loading}>Synchronizovat</SubmitButton>
      )}
    </div>
  );
}
