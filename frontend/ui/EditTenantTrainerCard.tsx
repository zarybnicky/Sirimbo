import { DeleteTenantTrainerDocument, TenantTrainerFragment, UpdateTenantTrainerDocument } from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, StdDialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { EditTenantTrainerForm } from "@/ui/forms/EditTenantTrainerForm";
import { useAuth } from "@/ui/use-auth";
import { MoreHorizontal } from "lucide-react";
import Link from "next/link";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function EditTenantTrainerCard({data, showPerson}: { data: TenantTrainerFragment; showPerson?: boolean }) {
  const auth = useAuth();
  const update = useMutation(UpdateTenantTrainerDocument)[1];
  const doRemove = useMutation(DeleteTenantTrainerDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete ${data.person?.name} ukončit trenérství ke dnešnímu datu?`})
    await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Ukončeno");
  }, [update, data.id, confirm, data.person?.name]);

  const remove = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete trenéra NENÁVRATNĚ smazat, včetně všech odučených lekcí? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.` });
    await doRemove({ id: data.id });
    toast.success("Odstraněno");
  }, [confirm, doRemove, data]);

  return (
    <div className="flex gap-3 mb-1 align-baseline">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10"/>
          </DropdownMenuTrigger>

          <DropdownMenuContent align="start">
            <Dialog>
              <StdDialogTrigger.Dropdown text="Upravit trenéra" />
              <DialogContent>
                <EditTenantTrainerForm id={data.id}/>
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
          <b>Trenér v klubu {data.tenant?.name}</b>
        )}

        {auth.isAdmin && (
          <div className="flex flex-wrap gap-4">
            <span>
              {data.memberPrice45Min?.amount ?? '- '}
              {'Kč '}
              {data.guestPrice45Min ? ('(' + data.guestPrice45Min.amount + 'Kč)') : ''}
              {' / 45min'}
            </span>
          </div>
        )}
      </div>
    </div>
  );
}
