import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  type CoupleFragment,
  DeleteCoupleDocument,
  UpdateCoupleDocument,
} from '@/graphql/Memberships';
import { Action } from '@/lib/actions';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';
import { formatLongCoupleName } from '@/ui/format';

export const coupleActions: Action<CoupleFragment>[] = [
  {
    id: 'couple.edit',
    primary: true,
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <EditCoupleForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-2xl',
    },
  },
  {
    id: 'couple.endToday',
    label: 'Ukončit k dnešnímu datu',
    icon: Unplug,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: ({ item }) =>
      `Opravdu chcete partnerství ${formatLongCoupleName(item)} ukončit ke dnešnímu datu?`,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateCoupleDocument, {
        input: {
          id: item.id,
          patch: { until: new Date().toISOString() },
        },
      });
    },
  },
  {
    id: 'couple.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm:
      'Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.',
    execute: async ({ item, mutate, router }) => {
      await mutate(DeleteCoupleDocument, { id: item.id });
      if (router.pathname === '/pary/[id]') {
        await router.replace('/pary');
      }
    },
  },
];
