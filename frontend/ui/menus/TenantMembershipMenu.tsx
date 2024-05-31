import {
  DeleteTenantMembershipDocument,
  TenantMembershipFragment,
  UpdateTenantMembershipDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditTenantMembershipForm } from '@/ui/forms/EditTenantMembershipForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { useAuth } from '../use-auth';

export function TenantMembershipMenu({
  data,
  children,
  ...props
}: { data: TenantMembershipFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateTenantMembershipDocument)[1];
  const doRemove = useMutation(DeleteTenantMembershipDocument)[1];
  const confirm = useConfirm();
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);

  const endToday = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?`,
    });
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } } });
    toast.success('Členství ukončeno');
  }, [confirm, update, data.id, data.person?.name]);

  const remove = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.`,
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
        <DropdownMenuButton onSelect={() => setEditOpen(true)}>Upravit členství</DropdownMenuButton>
        <DropdownMenuButton onSelect={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
        <DropdownMenuButton onSelect={remove}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantMembershipForm id={data.id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
