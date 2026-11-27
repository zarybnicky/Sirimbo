import { Pencil, Settings } from 'lucide-react';
import { createElement } from 'react';
import { defineActions } from '@/lib/actions';
import { TenantForm } from '@/ui/forms/TenantForm';
import { TenantSettingsForm } from '@/ui/forms/TenantSettingsForm';

export const tenantActions = defineActions<{ id: string }>()([
  {
    id: 'tenant.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.canManageTenant({ tenantId: item.id }),
    render: ({ item }) => createElement(TenantForm, { tenantId: item.id }),
  },
  {
    id: 'tenant.editSettings',
    label: 'Upravit nastavení',
    icon: Settings,
    requireSystemAdmin: true,
    render: ({ item }) => createElement(TenantSettingsForm, { tenantId: item.id }),
  },
]);
