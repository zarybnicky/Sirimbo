import { Trash2 } from 'lucide-react';
import { Action } from '@/lib/actions';
import { DeleteTransactionDocument } from '@/graphql/Payment';

export const transactionActions: Action<{ id: string }>[] = [
  {
    id: 'transaction.delete',
    label: 'Smazat transakci',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: 'Opravdu chcete smazat transakci?',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteTransactionDocument, { id: item.id });
    },
  },
];
