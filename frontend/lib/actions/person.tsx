import { Pencil, Trash2, UserPlus } from 'lucide-react';
import { DeletePersonDocument, type PersonFragment } from '@/graphql/Person';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { Action } from '@/lib/actions';

export const personActions: Action<PersonFragment>[] = [
  {
    id: 'person.edit',
    primary: true,
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.isAdmin || auth.isMyPerson(item.id),
    type: 'dialog',
    render: ({ item }) => <EditPersonForm data={item} />,
    dialogProps: {
      className: 'sm:max-w-2xl',
      onPointerDownOutside: (e) => e.preventDefault(),
    },
  },
  {
    id: 'person.createCouple',
    label: 'Přidat pár',
    icon: UserPlus,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <CreateCoupleForm person={item} />,
  },
  {
    id: 'person.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth, item }) => auth.isAdmin && !item.externalIds,
    type: 'mutation',
    confirm: ({ item }) =>
      `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item?.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!`,
    execute: async ({ item, mutate, router }) => {
      await mutate(DeletePersonDocument, { id: item.id });
      if (router.pathname === '/clenove/[id]') {
        await router.replace('/clenove');
      }
    },
  },
];
