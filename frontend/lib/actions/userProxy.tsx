import { LogIn, Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteUserProxyDocument,
  UpdateUserProxyDocument,
  type UserProxyFragment,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditUserProxyForm } from '@/ui/forms/EditUserProxyForm';
import { authAtom, storeRef } from '@/lib/auth';

export const userProxyActions = defineActions<UserProxyFragment>()([
  {
    id: 'userProxy.edit',
    label: ({ item }) => ['Upravit účet', item.user?.uEmail].filter(Boolean).join(' · '),
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
    execute: async ({ item, router }) => {
      if (!item.user) return;
      const response = await fetch('/api/auth/log-in-as', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ id: item.user.id }),
      });
      if (!response.ok) throw new Error('Přihlášení selhalo');

      storeRef.current.set(authAtom, { claims: null, user: null });
      storeRef.resetUrqlClient();
      router.replace('/dashboard');
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
    label: 'Smazat',
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
