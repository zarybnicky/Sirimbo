import { Link2, LogIn, Mail } from 'lucide-react';
import { logInAsAction } from '@/lib/auth-actions';
import { ResetPasswordDocument } from '@/graphql/CurrentUser';
import { toast } from 'react-toastify';
import { defineActions } from '@/lib/actions';
import { LinkPersonToUserForm } from '@/ui/forms/LinkPersonToUserForm';

export const userActions = defineActions<{ id: string; uEmail: string }>()([
  {
    id: 'user.logInAs',
    label: 'Přihlásit se jako...',
    icon: LogIn,
    requireAdmin: true,
    execute: async ({ item }) => {
      const result = await logInAsAction(item.id);
      if ('error' in result) throw new Error(result.error);

      window.location.replace(result.redirectTo);
    },
  },
  {
    id: 'user.resetPassword',
    label: 'Odeslat obnovu hesla',
    icon: Mail,
    requireAdmin: true,
    confirm: 'Odeslat uživateli e-mail pro obnovu hesla?',
    execute: async ({ item, mutate }) => {
      await mutate(ResetPasswordDocument, { input: { email: item.uEmail } });
      toast.success('E-mail pro obnovu hesla byl zařazen k odeslání.');
    },
  },
  {
    id: 'user.linkPerson',
    group: 'primary',
    label: 'Přiřadit osobu',
    icon: Link2,
    requireAdmin: true,
    render: ({ item }) => <LinkPersonToUserForm userId={item.id} />,
  },
]);
