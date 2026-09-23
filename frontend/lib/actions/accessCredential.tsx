import { Unplug } from 'lucide-react';
import {
  EndAccessCredentialDocument,
  type AccessCredentialFragment,
} from '@/graphql/AccessCredential';
import { defineActions } from '@/lib/actions';

export const accessCredentialActions = defineActions<AccessCredentialFragment>()([
  {
    id: 'accessCredential.revoke',
    label: 'Zneplatnit kartu',
    icon: Unplug,
    requireAdmin: true,
    requireStarletImport: true,
    visible: ({ item }) => !item.until || new Date(item.until) > new Date(),
    confirm: 'Opravdu chcete zneplatnit tuto kartu?',
    execute: async ({ item, mutate }) => {
      await mutate(EndAccessCredentialDocument, {
        id: item.id,
        until: new Date().toISOString(),
      });
    },
  },
]);
