import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteTenantMembershipDocument,
  type TenantMembershipFragment,
  UpdateTenantMembershipDocument,
} from '@/graphql/Memberships';
import { Action } from '@/lib/actions';
import { EditTenantMembershipForm } from '@/ui/forms/EditTenantMembershipForm';

export const tenantMembershipActions: Action<TenantMembershipFragment>[] = [
  {
    id: 'tenantMembership.edit',
    label: 'Upravit členství',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <EditTenantMembershipForm id={item.id} />,
  },
  {
    id: 'tenantMembership.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: ({ item }) =>
      `Opravdu chcete členovi ${item.person?.name} ukončit členství ke dnešnímu datu?`,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateTenantMembershipDocument, {
        input: { id: item.id, patch: { until: new Date().toISOString() } },
      });
    },
  },
  {
    id: 'tenantMembership.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm:
      'Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteTenantMembershipDocument, { id: item.id });
    },
  },
];
