import { LogIn, Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteUserProxyDocument,
  UpdateUserProxyDocument,
  type UserProxyFragment,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditUserProxyForm } from '@/ui/forms/EditUserProxyForm';
import { authAtom, storeRef } from '@/ui/state/auth';

export const userProxyActions = defineActions<UserProxyFragment>()([
  {
    id: 'userProxy.edit',
    label: 'Upravit platnost',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    render: ({ item }) => <EditUserProxyForm id={item.id} />,
  },
  {
    id: 'userProxy.logInAs',
    label: 'Přihlásit se jako...',
    icon: LogIn,
    visible: ({ auth, item }) => auth.isAdmin && !!item.user,
    execute: async ({ item, router }) => {
      if (!item.user) return;
      const response = await fetch('/api/auth/log-in-as', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ id: item.user.id }),
      });
      if (!response.ok) throw new Error('Přihlášení selhalo');

      storeRef.current.set(authAtom, null, null);
      storeRef.resetUrqlClient();
      await router.replace('/dashboard');
    },
  },
  {
    id: 'userProxy.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
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
    visible: ({ auth }) => auth.isAdmin,
    confirm:
      'Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteUserProxyDocument, { id: item.id });
    },
  },
]);
