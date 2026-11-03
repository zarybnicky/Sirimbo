import { LogIn, Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteUserProxyDocument,
  UpdateUserProxyDocument,
  type UserProxyFragment,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { logInAsAction } from '@/lib/auth-actions';
import { EditUserProxyForm } from '@/ui/forms/EditUserProxyForm';

export const userProxyActions = defineActions<UserProxyFragment>()([
  {
    id: 'userProxy.edit',
    label: ({ item }) => ['Upravit přístup', item.user?.uEmail].filter(Boolean).join(' · '),
    icon: Pencil,
    requireAdmin: true,
    render: ({ item }) => <EditUserProxyForm id={item.id} />,
  },
  {
    id: 'userProxy.logInAs',
    label: 'Přihlásit se jako...',
    icon: LogIn,
    requireAdmin: true,
    visible: ({ item }) => !!item.user,
    execute: async ({ item }) => {
      if (!item.user) return;
      const result = await logInAsAction(item.user.id);
      if ('error' in result) throw new Error(result.error);

      window.location.replace(result.redirectTo);
    },
  },
  {
    id: 'userProxy.endToday',
    label: ({ item }) => ['Ukončit přístup', item.user?.uEmail].filter(Boolean).join(' · '),
    icon: Unplug,
    requireAdmin: true,
    visible: ({ item }) => item.status === 'ACTIVE',
    confirm: 'Opravdu chcete ukončit platnost těchto přihlašovacích údajů?',
    execute: async ({ item, mutate }) => {
      await mutate(UpdateUserProxyDocument, {
        input: { id: item.id, patch: { until: new Date().toISOString() } },
      });
    },
  },
  {
    id: 'userProxy.delete',
    label: 'Smazat přístup',
    icon: Trash2,
    variant: 'danger',
    requireAdmin: true,
    confirm:
      'Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteUserProxyDocument, { id: item.id });
    },
  },
]);
