import { Download, Pencil } from 'lucide-react';
import type { CohortFragment } from '@/graphql/Cohorts';
import { Action } from '@/lib/actions';
import { CohortForm } from '@/ui/forms/CohortForm';
import { exportCohort } from '@/ui/reports/export-cohort';

export type CohortActionItem = Pick<CohortFragment, 'id' | 'name'>;

export const cohortActions: Action<CohortActionItem>[] = [
  {
    id: 'cohort.export',
    label: 'Export členů',
    icon: Download,
    visible: ({ auth, item }) => auth.isTrainerOrAdmin && !!item.id,
    type: 'mutation',
    execute: async ({ item }) => {
      await exportCohort([item.id], item.name);
    },
  },
  {
    id: 'cohort.edit',
    primary: true,
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.isAdmin && !!item.id,
    type: 'dialog',
    render: ({ item }) => <CohortForm id={item.id} />,
  },
];
