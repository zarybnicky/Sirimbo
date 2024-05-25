import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { PersonWithLinksFragment } from '@/graphql/Person';
import { tenantId } from '@/tenant/config';
import { EditCohortMembershipCard } from "@/ui/EditCohortMembershipCard";
import { EditCoupleCard } from "@/ui/EditCoupleCard";
import { EditTenantAdministratorCard } from "@/ui/EditTenantAdministratorCard";
import { EditTenantMembershipCard } from "@/ui/EditTenantMembershipCard";
import { EditTenantTrainerCard } from "@/ui/EditTenantTrainerCard";
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { Plus } from 'lucide-react';
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
        <EditCoupleCard key={item.id} data={item} />
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
        <EditCohortMembershipCard key={item.id} data={item} />
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
        <EditTenantAdministratorCard key={item.id} data={item} />
      ))}
      {item.tenantTrainersList.filter(x => x.active).map((item) => (
        <EditTenantTrainerCard key={item.id} data={item} />
      ))}
      {item.tenantMembershipsList.map((item) => (
        <EditTenantMembershipCard key={item.id} data={item} />
      ))}
    </div>
  );
}
