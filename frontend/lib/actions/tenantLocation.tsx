import { Pencil } from 'lucide-react';
import { Action } from '@/lib/actions';
import { EditTenantLocationForm } from '@/ui/forms/EditLocationForm';

export const tenantLocationActions: Action<{ id: string }>[] = [
  {
    id: 'tenantLocation.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <EditTenantLocationForm id={item.id} />,
  },
];
