import {
    DeleteTenantAdministratorDocument,
    TenantAdministratorFragment,
    UpdateTenantAdministratorDocument
} from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, StdDialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { EditTenantAdministratorForm } from "@/ui/forms/EditTenantAdministratorForm";
import { useAuth } from "@/ui/use-auth";
import { MoreHorizontal } from "lucide-react";
import Link from "next/link";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function EditTenantAdministratorCard({data, showPerson}: {
  data: TenantAdministratorFragment;
  showPerson?: boolean;
}) {
  const auth = useAuth();
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
    <div className="flex gap-3 mb-1">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10" />
          </DropdownMenuTrigger>

          <DropdownMenuContent align="start">
            <Dialog>
              <StdDialogTrigger.Dropdown text="Upravit správcovství" />
              <DialogContent>
                <EditTenantAdministratorForm id={data.id} />
              </DialogContent>
            </Dialog>
            <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
            <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
          </DropdownMenuContent>
        </DropdownMenu>
      )}

      <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
        {showPerson ? (
          <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>{data.person?.name}</Link>
        ) : (
          <b>Správce klubu {data.tenant?.name}</b>
        )}
      </div>
    </div>

  );
}
