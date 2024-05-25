import { CoupleFragment, DeleteCoupleDocument, UpdateCoupleDocument } from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { formatLongCoupleName, formatOpenDateRange } from "@/ui/format";
import { EditCoupleForm } from "@/ui/forms/EditCoupleForm";
import { useAuth } from "@/ui/use-auth";
import { MoreHorizontal } from "lucide-react";
import Link from "next/link";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function EditCoupleCard({data}: { data: CoupleFragment; }) {
  const auth = useAuth();
  const confirm = useConfirm();
  const doUpdate = useMutation(UpdateCoupleDocument)[1];
  const doRemove = useMutation(DeleteCoupleDocument)[1];

  const update = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete pár ${formatLongCoupleName(data)} ukončit ke dnešnímu datu?`})
    await doUpdate({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Ukončeno");
  }, [data, confirm, doUpdate]);

  const remove = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.`})
    await doRemove({id: data.id});
    toast.success("Ukončeno");
  }, [data, confirm, doRemove]);

  return (
    <div className="flex gap-3 mb-1 align-baseline">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10"/>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="start">
            <Dialog>
              <DialogTrigger.Dropdown text="Upravit partnerství" />
              <DialogContent>
                <EditCoupleForm id={data.id} />
              </DialogContent>
            </Dialog>
            <DropdownMenuButton onClick={update}>Ukončit ke dnešnímu datu</DropdownMenuButton>
            <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
          </DropdownMenuContent>
        </DropdownMenu>
      )}

      <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
        <Link className="underline font-bold" href={`/pary/${data.id}`}>
          {formatLongCoupleName(data)}
          </Link>
        <span>{formatOpenDateRange(data)}</span>
      </div>
    </div>
  );
}
