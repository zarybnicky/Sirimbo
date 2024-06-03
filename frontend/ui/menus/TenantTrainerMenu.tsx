import {
  DeleteTenantTrainerDocument,
  type TenantTrainerFragment,
  UpdateTenantTrainerDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditTenantTrainerForm } from '@/ui/forms/EditTenantTrainerForm';
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { useAuth } from '../use-auth';

export function TenantTrainerMenu({
  data,
  children,
  ...props
}: { data: TenantTrainerFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateTenantTrainerDocument)[1];
  const doRemove = useMutation(DeleteTenantTrainerDocument)[1];
  const confirm = useConfirm();
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);

  const endToday = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete ${data.person?.name} ukončit trenérství ke dnešnímu datu?`,
    });
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } } });
    toast.success('Ukončeno');
  }, [update, data.id, confirm, data.person?.name]);

  const remove = React.useCallback(async () => {
    await confirm({
      description: 'Opravdu chcete trenéra NENÁVRATNĚ smazat, včetně všech odučených lekcí? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.',
    });
    await doRemove({ id: data.id });
    toast.success('Odstraněno');
  }, [confirm, doRemove, data]);

  if (!auth.isAdmin) {
    return null;
  }

  return (
    <DropdownMenu>
      {children}

      <DropdownMenuContent {...props}>
        <DropdownMenuButton onClick={() => setEditOpen(true)}>
          Upravit trenéra
        </DropdownMenuButton>

        <DropdownMenuButton onClick={endToday}>
          Ukončit ke dnešnímu datu
        </DropdownMenuButton>
        <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantTrainerForm id={data.id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
