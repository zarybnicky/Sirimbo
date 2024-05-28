import {
  DeleteTenantAdministratorDocument,
  TenantAdministratorFragment,
  UpdateTenantAdministratorDocument
} from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenuButton, DropdownMenuContent } from "@/ui/dropdown";
import { EditTenantAdministratorForm } from "@/ui/forms/EditTenantAdministratorForm";
import { DropdownMenuContentProps } from "@radix-ui/react-dropdown-menu";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function TenantAdministratorMenu({ data, ...props }: { data: TenantAdministratorFragment } & DropdownMenuContentProps) {
  const update = useMutation(UpdateTenantAdministratorDocument)[1];
  const doRemove = useMutation(DeleteTenantAdministratorDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete ${data.person?.name} ukončit správcovství ke dnešnímu datu?`})
    await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Ukončeno");
  }, [confirm, data.id, data.person?.name, update]);

  const remove = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete vztah správce NENÁVRATNĚ smazat? Spíše použij variantu ukončení, ať zůstanou zachována historická data.`})
    await doRemove({id: data.id});
    toast.success("Odstraněno");
  }, [confirm, data.id, doRemove]);

  return (
    <DropdownMenuContent {...props}>
      <Dialog>
        <DialogTrigger.Dropdown text="Upravit správcovství" />
        <DialogContent>
          <EditTenantAdministratorForm id={data.id} />
        </DialogContent>
      </Dialog>
      <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
      <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
    </DropdownMenuContent>
  );
}
