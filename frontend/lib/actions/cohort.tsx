import { Download, Pencil } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { CohortForm } from '@/ui/forms/CohortForm';
import { exportCohort } from '@/ui/reports/export-cohort';

type CohortActionItem = {
  id: string;
  name: string;
};

export const cohortActions = defineActions<CohortActionItem>()([
  {
    id: 'cohort.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.isAdmin && !!item.id,
    type: 'dialog',
    render: ({ item }) => <CohortForm id={item.id} />,
  },
  {
    id: 'cohort.export',
    label: 'Export členů',
    icon: Download,
    visible: ({ auth, item }) => auth.isTrainerOrAdmin && !!item.id,
    type: 'mutation',
    execute: async ({ item, client }) => {
      await exportCohort(client, [item.id], item.name);
    },
  },
]);
