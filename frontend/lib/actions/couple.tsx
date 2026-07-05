import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  type CoupleFragment,
  DeleteCoupleDocument,
  UpdateCoupleDocument,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';
import { formatLongCoupleName } from '@/ui/format';
import { route } from 'nextjs-routes';

export const coupleActions = defineActions<CoupleFragment>()([
  {
    id: 'couple.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth }) => auth.isAdmin,
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
    confirm:
      'Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.',
    execute: async ({ item: { id }, mutate, router }) => {
      await mutate(DeleteCoupleDocument, { id });
      if (router.pathname === route({ pathname: '/pary/[id]', query: { id } })) {
        await router.replace('/pary');
      }
    },
  },
]);
