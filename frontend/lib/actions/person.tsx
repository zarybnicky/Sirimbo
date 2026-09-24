import { Coins, Link2, MailPlus, Pencil, Plus, Trash2, UserPlus } from 'lucide-react';
import { DeletePersonDocument, type PersonBasicFragment } from '@/graphql/Person';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { defineActions } from '@/lib/actions';
import { CreateCreditTransactionForm } from '@/ui/forms/CreateCreditTransactionForm';
import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
  CreateUserProxyDocument,
} from '@/graphql/Memberships';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { LinkUserToPersonForm } from '@/ui/forms/LinkUserToPersonForm';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { DialogTitle } from '@/ui/dialog';

export type PersonActionItem = PersonBasicFragment & {
  canInvite?: boolean;
  matchingUserId?: string;
};

export const personActions = defineActions<PersonActionItem>()([
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
    id: 'person.linkUser',
    label: 'Přiřadit existující účet',
    icon: Link2,
    requireAdmin: true,
    render: ({ item }) => <LinkUserToPersonForm person={item} />,
  },
  {
    id: 'person.assignUserByEmail',
    label: 'Přiřadit účet podle e-mailu',
    icon: Link2,
    requireAdmin: true,
    visible: ({ item }) => !!item.matchingUserId,
    execute: async ({ item, mutate }) => {
      await mutate(CreateUserProxyDocument, {
        input: {
          userProxy: { personId: item.id, userId: item.matchingUserId! },
        },
      });
    },
  },
  {
    id: 'person.invite',
    label: 'Pozvat e-mailem',
    icon: MailPlus,
    requireAdmin: true,
    visible: ({ item }) => item.canInvite !== false,
    render: ({ item }) => <CreateInvitationForm person={item} />,
  },
  {
    id: 'person.addAccessCredential',
    group: 'add',
    label: 'Přidat přístupovou kartu',
    icon: UserPlus,
    requireAdmin: true,
    requireStarletImport: true,
    render: ({ item }) => (
      <>
        <DialogTitle>Přidat kartu</DialogTitle>
        <AccessCredentialForm personId={item.id} />
      </>
    ),
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
