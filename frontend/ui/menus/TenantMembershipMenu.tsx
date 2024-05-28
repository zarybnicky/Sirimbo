import {
  DeleteTenantMembershipDocument,
  TenantMembershipFragment,
  UpdateTenantMembershipDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditTenantMembershipForm } from '@/ui/forms/EditTenantMembershipForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

export function TenantMembershipMenu({
  data,
  ...props
}: { data: TenantMembershipFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateTenantMembershipDocument)[1];
  const doRemove = useMutation(DeleteTenantMembershipDocument)[1];
  const confirm = useConfirm();

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

  return (
    <DropdownMenuContent {...props}>
      <Dialog>
        <DialogTrigger.Dropdown text="Upravit členství" />
        <DialogContent>
          <EditTenantMembershipForm id={data.id} />
        </DialogContent>
      </Dialog>
      <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
      <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
    </DropdownMenuContent>
  );
}
