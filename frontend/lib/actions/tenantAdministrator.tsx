import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteTenantAdministratorDocument,
  type TenantAdministratorFragment,
  UpdateTenantAdministratorDocument,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditTenantAdministratorForm } from '@/ui/forms/EditTenantAdministratorForm';

export const tenantAdministratorActions = defineActions<TenantAdministratorFragment>()([
  {
    id: 'tenantAdministrator.edit',
    label: 'Upravit správcovství',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    render: ({ item }) => <EditTenantAdministratorForm id={item.id} />,
  },
  {
    id: 'tenantAdministrator.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    confirm: ({ item }) =>
      `Opravdu chcete ${item.person?.name} ukončit správcovství ke dnešnímu datu?`,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateTenantAdministratorDocument, {
        input: { id: item.id, patch: { until: new Date().toISOString() } },
      });
    },
  },
  {
    id: 'tenantAdministrator.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    confirm:
      'Opravdu chcete vztah správce NENÁVRATNĚ smazat? Spíše použij variantu ukončení, ať zůstanou zachována historická data.',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteTenantAdministratorDocument, { id: item.id });
    },
  },
]);
