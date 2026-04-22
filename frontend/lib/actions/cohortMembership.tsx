import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteCohortMembershipDocument,
  UpdateCohortMembershipDocument,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditCohortMembershipForm } from '@/ui/forms/EditCohortMembershipForm';

type Item = {
  __typename?: 'CohortMembership';
  id: string;
  person?: {
    name: string;
  } | null;
};

export const cohortMembershipActions = defineActions<Item>()([
  {
    id: 'cohortMembership.edit',
    label: 'Upravit členství',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <EditCohortMembershipForm id={item.id} />,
  },
  {
    id: 'cohortMembership.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: ({ item }) =>
      `Opravdu chcete členovi ${item.person?.name} ukončit členství ke dnešnímu datu?`,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateCohortMembershipDocument, {
        input: { id: item.id, patch: { until: new Date().toISOString() } },
      });
    },
  },
  {
    id: 'cohortMembership.delete',
    label: 'Smazat členství',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm:
      'Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteCohortMembershipDocument, { id: item.id });
    },
  },
]);
