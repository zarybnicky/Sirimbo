import { LogIn, Pencil, Trash2, Unplug } from 'lucide-react';
import {
  DeleteUserProxyDocument,
  UpdateUserProxyDocument,
  type UserProxyFragment,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { sessionPresentAtom, storeRef } from '@/ui/state/auth';
import { EditUserProxyForm } from '@/ui/forms/EditUserProxyForm';

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
      const res = await fetch('/api/auth/log-in-as', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ id: item.user.id }),
      });
      if (!res.ok) return;
      // New session cookie is set server-side; refresh client auth state.
      storeRef.current.set(sessionPresentAtom, true);
      storeRef.resetUrqlClient?.();
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
