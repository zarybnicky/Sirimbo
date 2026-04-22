import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteTenantTrainerDocument,
  type TenantTrainerFragment,
  UpdateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditTenantTrainerForm } from '@/ui/forms/EditTenantTrainerForm';

export const tenantTrainerActions = defineActions<TenantTrainerFragment>()([
  {
    id: 'tenantTrainer.edit',
    label: 'Upravit trenéra',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    render: ({ item }) => <EditTenantTrainerForm id={item.id} />,
  },
  {
    id: 'tenantTrainer.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    confirm: ({ item }) =>
      `Opravdu chcete ${item.person?.name} ukončit trenérství ke dnešnímu datu?`,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateTenantTrainerDocument, {
        input: { id: item.id, patch: { until: new Date().toISOString() } },
      });
    },
  },
  {
    id: 'tenantTrainer.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    confirm:
      'Opravdu chcete trenéra NENÁVRATNĚ smazat, včetně všech odučených lekcí? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteTenantTrainerDocument, { id: item.id });
    },
  },
]);
