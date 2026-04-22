import { Coins, Pencil, Plus, Trash2, UserPlus } from 'lucide-react';
import { DeletePersonDocument, type PersonFragment } from '@/graphql/Person';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { defineActions } from '@/lib/actions';
import { CreateCreditTransactionForm } from '@/ui/forms/CreateCreditTransactionForm';
import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';

export const personActions = defineActions<PersonFragment>()([
  {
    id: 'person.edit',
    group: 'primary',
    label: 'Upravit osobu',
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
    id: 'person.createCreditTransaction',
    label: 'Přidat/vyplatit kredit',
    icon: Coins,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <CreateCreditTransactionForm personId={item.id} />,
  },
  {
    id: 'person.createCouple',
    group: 'add',
    label: 'Přidat do páru',
    icon: Plus,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <CreateCoupleForm person={item} />,
  },
  {
    id: 'person.addToCohort',
    group: 'add',
    label: 'Přidat do skupiny',
    icon: Plus,
    visible: ({ auth }) => auth.isAdmin,
    type: 'dialog',
    render: ({ item }) => <AddToCohortForm person={item} />,
  },
  {
    id: 'person.addMember',
    group: 'add',
    label: 'Přidat jako člena',
    icon: UserPlus,
    visible: ({ auth, item }) => auth.isAdmin && !item.isMember,
    type: 'mutation' as const,
    execute: async ({ item, mutate }) => {
      await mutate(CreateTenantMembershipDocument, {
        input: { tenantMembership: { personId: item.id } },
      });
    },
  },
  {
    id: 'person.addTrainer',
    group: 'add',
    label: 'Přidat jako trenéra',
    icon: UserPlus,
    visible: ({ auth, item }) => auth.isAdmin && !item.isTrainer,
    type: 'mutation' as const,
    execute: async ({ item, mutate }) => {
      await mutate(CreateTenantTrainerDocument, {
        input: { tenantTrainer: { personId: item.id } },
      });
    },
  },
  {
    id: 'person.addAdmin',
    group: 'add',
    label: 'Přidat jako správce',
    icon: UserPlus,
    visible: ({ auth, item }) => auth.isAdmin && !item.isAdmin,
    type: 'mutation' as const,
    execute: async ({ item, mutate }) => {
      await mutate(CreateTenantAdministratorDocument, {
        input: { tenantAdministrator: { personId: item.id } },
      });
    },
  },
  {
    id: 'person.delete',
    label: 'Smazat osobu',
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
]);
