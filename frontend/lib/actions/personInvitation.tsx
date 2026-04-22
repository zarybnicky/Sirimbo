import { Copy, Trash2 } from 'lucide-react';
import { DeleteInvitationDocument } from '@/graphql/Invitation';
import { defineActions } from '@/lib/actions';

export type PersonInvitationActionItem = {
  id: string;
  accessToken: string;
};

export const personInvitationActions = defineActions<PersonInvitationActionItem>()([
  {
    id: 'personInvitation.copyLink',
    label: 'Kopírovat odkaz',
    icon: Copy,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    execute: async ({ item }) => {
      await navigator.clipboard.writeText(
        `${window.location.origin}/pozvanka?token=${item.accessToken}`,
      );
    },
  },
  {
    id: 'personInvitation.delete',
    label: 'Zrušit pozvánku',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteInvitationDocument, { input: { id: item.id } });
    },
  },
]);
