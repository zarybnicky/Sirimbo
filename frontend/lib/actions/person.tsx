import { Coins, Pencil, Plus, Trash2, UserPlus } from 'lucide-react';
import { DeletePersonDocument, type PersonBasicFragment } from '@/graphql/Person';
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
import { CreateAccessCredentialForm } from '@/ui/forms/CreateAccessCredentialForm';

export const personActions = defineActions<PersonBasicFragment>()([
  {
    id: 'person.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.isAdmin || auth.isMyPerson(item.id),
    render: ({ item }) => <EditPersonForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-2xl',
      onPointerDownOutside: (e) => e.preventDefault(),
    },
  },
  {
    id: 'person.createCreditTransaction',
    label: 'Přidat/vyplatit kredit',
    icon: Coins,
    requireAdmin: true,
    render: ({ item }) => <CreateCreditTransactionForm personId={item.id} />,
  },
  {
    id: 'person.createCouple',
    group: 'add',
    label: 'Přidat do páru',
    icon: Plus,
    requireAdmin: true,
    render: ({ item }) => <CreateCoupleForm person={item} />,
  },
  {
    id: 'person.addToCohort',
    group: 'add',
    label: 'Přidat do skupiny',
    icon: Plus,
    requireAdmin: true,
    render: ({ item }) => <AddToCohortForm person={item} />,
  },
  {
    id: 'person.addMember',
    group: 'add',
    label: 'Přidat jako člena',
    icon: UserPlus,
    requireAdmin: true,
    visible: ({ item }) => !item.isMember,
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
    requireAdmin: true,
    visible: ({ item }) => !item.isTrainer,
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
    requireAdmin: true,
    visible: ({ item }) => !item.isAdmin,
    execute: async ({ item, mutate }) => {
      await mutate(CreateTenantAdministratorDocument, {
        input: { tenantAdministrator: { personId: item.id } },
      });
    },
  },
  {
    id: 'person.addAccessCredential',
    group: 'add',
    label: 'Přidat přístupovou kartu',
    icon: UserPlus,
    requireAdmin: true,
    requireStarletImport: true,
    render: ({ item }) => <CreateAccessCredentialForm personId={item.id} />,
  },
  {
    id: 'person.delete',
    label: 'Smazat osobu',
    icon: Trash2,
    variant: 'danger',
    requireAdmin: true,
    visible: ({ item }) => !item.externalIds,
    confirm: ({ item }) =>
      `Opravdu chcete NENÁVRATNĚ smazat uživatele a všechna jeho data "${item?.name}"? Toto udělejte pouze v případě, že jste při vytváření uživatele udělali chybu, finanční údaje dlouholetých členů potřebujeme nechat v evidenci!`,
    execute: async ({ item: { id }, mutate, router }) => {
      await mutate(DeletePersonDocument, { id });
      if (router.pathname === `/clenove/${id}`) {
        router.replace('/clenove');
      }
    },
  },
]);
