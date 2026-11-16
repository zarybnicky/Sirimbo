import { Pencil, Trash2, Unplug } from 'lucide-react';
import {
  type CoupleFragment,
  DeleteCoupleDocument,
  UpdateCoupleDocument,
} from '@/graphql/Memberships';
import { defineActions } from '@/lib/actions';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';

export const coupleActions = defineActions<CoupleFragment>()([
  {
    id: 'couple.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    requireAdmin: true,
    visible: ({ auth, item }) =>
      [item.man, item.woman].some((person) => person && auth.canManagePerson(person)),
    render: ({ item }) => <EditCoupleForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-2xl',
    },
  },
  {
    id: 'couple.endToday',
    label: 'Ukončit k dnešnímu datu',
    icon: Unplug,
    requireAdmin: true,
    visible: ({ auth, item }) => item.status === 'ACTIVE' &&
      [item.man, item.woman].some((person) => person && auth.canManagePerson(person)),
    confirm: ({ item }) =>
      `Opravdu chcete partnerství ${item.name} ukončit ke dnešnímu datu?`,
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
    requireAdmin: true,
    visible: ({ auth, item }) =>
      [item.man, item.woman].some((person) => person && auth.canManagePerson(person)),
    confirm:
      'Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.',
    execute: async ({ item: { id }, mutate, router }) => {
      await mutate(DeleteCoupleDocument, { id });
      if (router.pathname === `/pary/${id}`) {
        router.replace('/pary');
      }
    },
  },
]);
