import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { PersonWithLinksFragment } from '@/graphql/Person';
import { tenantId } from '@/tenant/config';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { CohortMembershipMenu } from '@/ui/menus/CohortMembershipMenu';
import { CoupleMenu } from '@/ui/menus/CoupleMenu';
import { TenantAdministratorMenu } from '@/ui/menus/TenantAdministratorMenu';
import { TenantMembershipMenu } from '@/ui/menus/TenantMembershipMenu';
import { TenantTrainerMenu } from '@/ui/menus/TenantTrainerMenu';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { Plus } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useMutation } from 'urql';

export function PersonMembershipView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();
  const createTenantMember = useMutation(CreateTenantMembershipDocument)[1];
  const createTenantTrainer = useMutation(CreateTenantTrainerDocument)[1];
  const createTenantAdmin = useMutation(CreateTenantAdministratorDocument)[1];

  const addAsMember = React.useCallback(() => createTenantMember({ input: { tenantMembership: { personId: item.id, tenantId } } }), [createTenantMember, item.id]);
  const addAsTrainer = React.useCallback(() => createTenantTrainer({ input: { tenantTrainer: { personId: item.id, tenantId } } }), [createTenantTrainer, item.id]);
  const addAsAdmin = React.useCallback(() => createTenantAdmin({ input: { tenantAdministrator: { personId: item.id, tenantId } } }), [createTenantAdmin, item.id]);

  return (
    <div key="info" className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Páry</h3>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <CreateCoupleForm initial={item} />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.allCouplesList?.map((item) => (
        <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
          <CoupleMenu align="start" data={item}>
            <DropdownMenuTrigger.RowDots />
          </CoupleMenu>

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <Link className="underline font-bold" href={`/pary/${item.id}`}>
              {formatLongCoupleName(item)}
            </Link>
            <span>{formatOpenDateRange(item)}</span>
          </div>
        </div>
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Tréninkové skupiny</h3>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <AddToCohortForm person={item} />
            </DialogContent>
          </Dialog>
        )}
      </div>
      {item.cohortMembershipsList.sort((x, y) => (x.person?.name || '').localeCompare(y.person?.name || '')).map((item) => (
        <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
          <CohortMembershipMenu align="start" data={item}>
            <DropdownMenuTrigger.RowDots />
          </CohortMembershipMenu>

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <b>
              Člen skupiny{' '}
              <Link className="underline font-bold" href={`/treninkove-skupiny/${item.cohort?.id}`}>
                {item.cohort?.name}
              </Link>
            </b>
            <span>{formatOpenDateRange(item)}</span>
          </div>
        </div>
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Členství</h3>

        {auth.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
              <Plus />
              Přidat
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuButton onClick={addAsAdmin}>jako správce</DropdownMenuButton>
              <DropdownMenuButton onClick={addAsTrainer}>jako trenéra</DropdownMenuButton>
              <DropdownMenuButton onClick={addAsMember}>jako člena</DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </div>

      {item.tenantAdministratorsList.map((item) => (
        <div className="flex gap-3 mb-1" key={item.id}>
          <TenantAdministratorMenu align="start" data={item}>
            <DropdownMenuTrigger.RowDots />
          </TenantAdministratorMenu>

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <b>Správce klubu {item.tenant?.name}</b>
          </div>
        </div>
      ))}
    
      {item.tenantTrainersList.filter(x => x.active).map((item) => (
        <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
          <TenantTrainerMenu align="start" data={item}>
            <DropdownMenuTrigger.RowDots />
          </TenantTrainerMenu>

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <b>Trenér v klubu {item.tenant?.name}</b>
            {auth.isAdmin && (
              <div>
                {item.memberPrice45Min?.amount ?? '- '}
                {'Kč '}
                {item.guestPrice45Min ? ('(' + item.guestPrice45Min.amount + 'Kč)') : ''}
                {' / 45min'}
              </div>
            )}
          </div>
        </div>
      ))}
      {item.tenantMembershipsList.map((item) => (
        <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
          <TenantMembershipMenu align="start" data={item}>
            <DropdownMenuTrigger.RowDots />
          </TenantMembershipMenu>

          <div className="grow align-baseline text-sm font-bold py-1">
            Člen klubu {item.tenant?.name}
          </div>
        </div>
      ))}
    </div>
  );
}
