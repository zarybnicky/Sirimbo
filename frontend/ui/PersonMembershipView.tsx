import React from 'react';
import { PersonWithLinksFragment } from '@/graphql/Person';
import { useMutation } from 'urql';
import { useAuth } from '@/ui/use-auth';
import { EditCohortMembershipCard } from '@/ui/EditCohortMembershipForm';
import { EditTenantAdministratorCard } from '@/ui/EditTenantAdministratorForm'
import { EditTenantTrainerCard } from '@/ui/EditTenantTrainerForm'
import { EditTenantMembershipCard } from '@/ui/EditTenantMembershipForm'
import { EditCoupleCard } from '@/ui/EditCoupleForm'
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from './dropdown';
import { buttonCls } from './style';
import { Plus } from 'lucide-react';
import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { tenantId } from '@/tenant/config';
import { AddToCohortForm } from './AddToCohortForm';
import { CreateCoupleForm } from './CreateCoupleForm';

export function PersonMembershipView({ item }: { item: PersonWithLinksFragment }) {
  const { perms } = useAuth();
  const [coupleOpen, setCoupleOpen] = React.useState(false);
  const [cohortOpen, setCohortOpen] = React.useState(false);
  const createTenantMember = useMutation(CreateTenantMembershipDocument)[1];
  const createTenantTrainer = useMutation(CreateTenantTrainerDocument)[1];
  const createTenantAdmin = useMutation(CreateTenantAdministratorDocument)[1];

  return (
    <div key="info" className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Páry</h3>

        {perms.isAdmin && (
          <Dialog open={coupleOpen} onOpenChange={setCoupleOpen} modal={false}>
            <DialogTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DialogTrigger>
            <DialogContent>
              <CreateCoupleForm initial={item} onSuccess={() => setCoupleOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.allCouplesList?.map((item) => (
        <EditCoupleCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Tréninkové skupiny</h3>

        {perms.isAdmin && (
          <Dialog open={cohortOpen} onOpenChange={setCohortOpen} modal={false}>
            <DialogTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DialogTrigger>
            <DialogContent>
              <AddToCohortForm person={item} onSuccess={() => setCohortOpen(false)} />
            </DialogContent>
          </Dialog>
        )}
      </div>
      {item.cohortMembershipsList.map((item) => (
        <EditCohortMembershipCard key={item.id} data={item} />
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Členství</h3>

        {perms.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <button className={buttonCls({ variant: 'outline', size: 'sm' })}>
                <Plus />
                Přidat
              </button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuButton
                onClick={() => createTenantAdmin({ input: { tenantAdministrator: { personId: item.id, tenantId } } })}
              >
                jako správce
              </DropdownMenuButton>
              <DropdownMenuButton
                onClick={() => createTenantTrainer({ input: { tenantTrainer: { personId: item.id, tenantId } } })}
              >
                jako trenéra
              </DropdownMenuButton>
              <DropdownMenuButton
                onClick={() => createTenantMember({ input: { tenantMembership: { personId: item.id, tenantId } } })}
              >
                jako člena
              </DropdownMenuButton>
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
