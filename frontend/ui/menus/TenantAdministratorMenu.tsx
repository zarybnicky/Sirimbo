import {
  DeleteTenantAdministratorDocument,
  TenantAdministratorFragment,
  UpdateTenantAdministratorDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditTenantAdministratorForm } from '@/ui/forms/EditTenantAdministratorForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { useAuth } from '../use-auth';

export function TenantAdministratorMenu({
  data,
  children,
  ...props
}: { data: TenantAdministratorFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateTenantAdministratorDocument)[1];
  const doRemove = useMutation(DeleteTenantAdministratorDocument)[1];
  const confirm = useConfirm();
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);

  const endToday = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete ${data.person?.name} ukončit správcovství ke dnešnímu datu?`,
    });
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } } });
    toast.success('Ukončeno');
  }, [confirm, data.id, data.person?.name, update]);

  const remove = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete vztah správce NENÁVRATNĚ smazat? Spíše použij variantu ukončení, ať zůstanou zachována historická data.`,
    });
    await doRemove({ id: data.id });
    toast.success('Odstraněno');
  }, [confirm, data.id, doRemove]);

  if (!auth.isAdmin) {
    return null;
  }

  return (
    <DropdownMenu>
      {children}

      <DropdownMenuContent {...props}>
        <DropdownMenuButton onSelect={() => setEditOpen(true)}>
          Upravit správcovství
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={endToday}>
          Ukončit ke dnešnímu datu
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={remove}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantAdministratorForm id={data.id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
