import { Copy, Mail, Trash2 } from 'lucide-react';
import { DeleteInvitationDocument, ResendInvitationDocument } from '@/graphql/Invitation';
import { defineActions } from '@/lib/actions';

export type PersonInvitationActionItem = {
  id: string;
  accessToken: string;
};

export const personInvitationActions = defineActions<PersonInvitationActionItem>()([
  {
    id: 'personInvitation.resend',
    label: 'Poslat znovu',
    icon: Mail,
    requireAdmin: true,
    execute: async ({ item, mutate }) => {
      await mutate(ResendInvitationDocument, { id: item.id });
    },
  },
  {
    id: 'personInvitation.copyLink',
    label: 'Kopírovat odkaz',
    icon: Copy,
    requireAdmin: true,
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
    requireAdmin: true,
    execute: async ({ item, mutate }) => {
      await mutate(DeleteInvitationDocument, { input: { id: item.id } });
    },
  },
]);
