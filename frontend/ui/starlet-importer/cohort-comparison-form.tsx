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
import { useMemo } from 'react';
import { EntityLink } from '../EntityLink';
import { FormError } from '@/ui/form';
import { ListHeader } from '@/ui/ListHeader';
import { Table } from '@/ui/Table';

type CohortTask =
  | { type: 'create'; name: string; description: string; externalId: string }
  | { type: 'update'; id: string; externalId: string }
  | { type: 'archive'; id: string };

const actionLabels = {
  create: 'Vytvoří',
  update: 'Změní ID z evidence',
  archive: 'Archivuje',
};

export function CohortComparisonForm() {
  const [cohortQuery] = useQuery({
    query: CohortListDocument,
  });
  const cohorts = cohortQuery.data?.cohortsList;
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
    <div className="not-prose text-neutral-12">
      <FormError error={onSubmit.error} />
      {tasks.length > 0 && (
        <ListHeader
          actions={
            <SubmitButton
              size="sm"
              action={onSubmit}
              disabled={cohortQuery.fetching || !!cohortQuery.error}
            >
              Synchronizovat vše
            </SubmitButton>
          }
        />
      )}
      <Table
        columns={['Evidence', 'Databáze', tasks.length > 0 && 'Akce']}
        busy={cohortQuery.fetching}
        error={cohortQuery.error}
        empty="Žádné kurzy."
      >
        {views.map(({ course, cohort, action }) => (
          <tr key={course?.id ?? cohort!.id} className="align-top">
            <td>
              {course && `${course.code}: ${course.name}`}
            </td>
            <td>{cohort && <EntityLink type="cohort" entity={cohort} />}</td>
            {tasks.length > 0 && (
              <td className="w-px whitespace-nowrap text-xs text-neutral-11">
                {action && actionLabels[action]}
              </td>
            )}
          </tr>
        ))}
      </Table>
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
  const views: {
    course?: StarletCourse;
    cohort?: (typeof cohorts)[number];
    action?: CohortTask['type'];
  }[] = [];
  const tasks: CohortTask[] = [];

  for (const { id, code, name } of courses) {
    const cohort = cohorts.find((cohort) => cohort.name === code);
    const needsUpdate = !!cohort && !cohort.externalIds?.includes(id);
    let action: CohortTask['type'] | undefined;
    if (!cohort) {
      tasks.push({ type: 'create', name: code, description: name, externalId: id });
      action = 'create';
    } else if (needsUpdate) {
      tasks.push({ type: 'update', id: cohort.id, externalId: id });
      action = 'update';
    }
    views.push({ course: { id, code, name }, cohort, action });
  }
  for (const cohort of cohorts) {
    if (courses.some((course) => cohort.name === course.code)) continue;
    if (!cohort.isVisible || !cohort.externalIds || cohort.externalIds.length === 0)
      continue;
    tasks.push({ type: 'archive', id: cohort.id });
    views.push({ cohort, action: 'archive' });
  }

  return [tasks, views] as const;
}
