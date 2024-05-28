import {
  CohortMembershipFragment,
  DeleteCohortMembershipDocument,
  UpdateCohortMembershipDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditCohortMembershipForm } from '@/ui/forms/EditCohortMembershipForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

export function CohortMembershipMenu({
  data,
  ...props
}: { data: CohortMembershipFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const del = useMutation(DeleteCohortMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?`,
    });
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } } });
    toast.success('Členství ukončeno');
  }, [update, confirm, data]);

  const remove = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.`,
    });
    await del({ id: data.id });
    toast.success('Odstraněno');
  }, [confirm, del, data.id]);

  return (
    <DropdownMenuContent {...props}>
      <Dialog>
        <DialogTrigger.Dropdown text="Upravit členství" />
        <DialogContent>
          <EditCohortMembershipForm id={data.id} />
        </DialogContent>
      </Dialog>
      <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
      <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
    </DropdownMenuContent>
  );
}
