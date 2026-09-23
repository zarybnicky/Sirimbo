import { Pencil } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';

export const tenantLocationActions = defineActions<{ id: string }>()([
  {
    id: 'tenantLocation.edit',
    label: 'Upravit',
    icon: Pencil,
    requireAdmin: true,
    render: ({ item }) => <EditTenantLocationForm id={item.id} />,
  },
]);
