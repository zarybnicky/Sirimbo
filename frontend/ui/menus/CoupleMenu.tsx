import {
  CoupleFragment,
  DeleteCoupleDocument,
  UpdateCoupleDocument,
} from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { formatLongCoupleName } from '@/ui/format';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

export function CoupleMenu({
  data,
  children,
  ...props
}: { data: CoupleFragment } & DropdownMenuContentProps) {
  const confirm = useConfirm();
  const doUpdate = useMutation(UpdateCoupleDocument)[1];
  const doRemove = useMutation(DeleteCoupleDocument)[1];

  const [editOpen, setEditOpen] = React.useState(false);

  const update = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete pár ${formatLongCoupleName(data)} ukončit ke dnešnímu datu?`,
    });
    await doUpdate({
      input: { id: data.id, patch: { until: new Date().toISOString() } },
    });
    toast.success('Ukončeno');
  }, [data, confirm, doUpdate]);

  const remove = React.useCallback(async () => {
    await confirm({
      description: `Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.`,
    });
    await doRemove({ id: data.id });
    toast.success('Ukončeno');
  }, [data, confirm, doRemove]);

  return (
    <DropdownMenu>
      {children}

      <DropdownMenuContent {...props}>
        <DropdownMenuButton onSelect={() => setEditOpen(true)}>Upravit partnerství</DropdownMenuButton>
        <DropdownMenuButton onSelect={update}>Ukončit ke dnešnímu datu</DropdownMenuButton>
        <DropdownMenuButton onSelect={remove}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditCoupleForm id={data.id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
