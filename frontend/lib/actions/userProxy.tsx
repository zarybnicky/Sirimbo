import { LogIn, Pencil, Trash2, Unplug } from 'lucide-react';
import { LogInAsDocument } from '@/graphql/CurrentUser';
import {
  DeleteUserProxyDocument,
  UpdateUserProxyDocument,
  type UserProxyFragment,
} from '@/graphql/Memberships';
import { Action } from '@/lib/actions';
import { EditUserProxyForm } from '@/ui/forms/EditUserProxyForm';

export const userProxyActions: Action<UserProxyFragment>[] = [
  {
    id: 'userProxy.edit',
    label: 'Upravit platnost',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <EditUserProxyForm id={item.id} />,
  },
  {
    id: 'userProxy.logInAs',
    label: 'Přihlásit se jako...',
    icon: LogIn,
    visible: ({ auth, item }) => auth.isAdmin && !!item.user,
    type: 'mutation',
    execute: async ({ item, mutate, router }) => {
      if (!item.user) return;
      await mutate(LogInAsDocument, { id: item.user.id });
      await router.replace('/dashboard');
    },
  },
  {
    id: 'userProxy.endToday',
    label: 'Ukončit ke dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
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
    type: 'mutation',
    confirm:
      'Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteUserProxyDocument, { id: item.id });
    },
  },
];
