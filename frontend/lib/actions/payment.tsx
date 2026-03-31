import { HandCoins, Info, Trash2 } from 'lucide-react';
import { Action } from '@/lib/actions';
import { DeletePaymentDocument, MarkAsPaidDocument } from '@/graphql/Payment';
import { PaymentStatus } from '@/graphql';

export const paymentActions: Action<{ id: string; status: PaymentStatus }>[] = [
  {
    id: 'payment.detail',
    label: 'Detail',
    icon: Info,
    visible: () => true,
    type: 'mutation',
    execute: async ({ item, router }) => {
      await router.push({ pathname: '/platby/[id]', query: { id: item.id } });
    },
  },
  {
    id: 'payment.markAsPaid',
    label: 'Označit jako zaplacenou',
    icon: HandCoins,
    visible: ({ auth, item }) => auth.isAdmin && item.status === 'UNPAID',
    type: 'mutation',
    execute: async ({ item, mutate }) => {
      await mutate(MarkAsPaidDocument, { id: item.id });
    },
  },
  {
    id: 'payment.delete',
    label: 'Smazat platbu',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: 'Opravdu chcete smazat platbu?',
    execute: async ({ item, mutate }) => {
      await mutate(DeletePaymentDocument, { id: item.id });
    },
  },
];
