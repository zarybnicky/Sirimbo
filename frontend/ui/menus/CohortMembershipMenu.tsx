import {
  CohortMembershipFragment,
  DeleteCohortMembershipDocument,
  UpdateCohortMembershipDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { EditCohortMembershipForm } from '@/ui/forms/EditCohortMembershipForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { useAuth } from '../use-auth';

export function CohortMembershipMenu({
  data,
  children,
  ...props
}: { data: CohortMembershipFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const del = useMutation(DeleteCohortMembershipDocument)[1];
  const confirm = useConfirm();
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);

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

  if (!auth.isAdmin) return null;

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
          <EditCohortMembershipForm id={data.id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
