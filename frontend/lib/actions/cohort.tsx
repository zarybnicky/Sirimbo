import { Copy, Download, Pencil } from 'lucide-react';
import { PersonListDocument } from '@/graphql/Person';
import { defineActions } from '@/lib/actions';
import { CohortForm } from '@/ui/forms/CohortForm';
import { exportCohort } from '@/ui/reports/export-cohort';
import { toast } from 'react-toastify';

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
    render: ({ item }) => <CohortForm id={item.id} />,
  },
  {
    id: 'cohort.export',
    label: 'Export členů',
    icon: Download,
    visible: ({ auth, item }) => auth.isTrainerOrAdmin && !!item.id,
    execute: async ({ item, client }) => {
      await exportCohort(client, [item.id], item.name);
    },
  },
  {
    id: 'cohort.copyEmails',
    label: 'Kopírovat e-maily',
    icon: Copy,
    visible: ({ auth, item }) => auth.isTrainerOrAdmin && !!item.id,
    execute: async ({ item, client }) => {
      const result = await client
        .query(PersonListDocument, { inCohorts: [item.id] })
        .toPromise();
      if (result.error) throw result.error;

      const emails = [
        ...new Set(
          (result.data?.people?.nodes ?? [])
            .map((person) => person.email?.trim()?.toLowerCase())
            .filter(Boolean),
        ),
      ];
      if (emails.length === 0) {
        toast.info('Ve skupině nemá nikdo vyplněný e-mail.');
        return;
      }

      await navigator.clipboard.writeText(emails.join(', '));
      toast.success('Zkopírováno');
    },
  },
]);
