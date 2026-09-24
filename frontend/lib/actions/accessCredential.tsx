import { Pencil, Unplug } from 'lucide-react';
import {
  EndAccessCredentialDocument,
  type AccessCredentialFragment,
} from '@/graphql/AccessCredential';
import { defineActions } from '@/lib/actions';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { DialogTitle } from '@/ui/dialog';

export const accessCredentialActions = defineActions<AccessCredentialFragment>()([
  {
    id: 'accessCredential.edit',
    label: ({ item }) => `Upravit kartu · ${item.label}`,
    icon: Pencil,
    requireAdmin: true,
    requireStarletImport: true,
    render: ({ item }) => (
      <>
        <DialogTitle>Upravit kartu</DialogTitle>
        <AccessCredentialForm credential={item} />
      </>
    ),
  },
  {
    id: 'accessCredential.revoke',
    label: ({ item }) => `Zneplatnit kartu · ${item.label}`,
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
