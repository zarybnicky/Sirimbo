import {CoupleFragment, DeleteCoupleDocument, UpdateCoupleDocument} from "@/graphql/Memberships";
import {useAuth} from "@/ui/use-auth";
import React from "react";
import {useMutation} from "urql";
import {useConfirm} from "@/ui/Confirm";
import {DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger} from "@/ui/dropdown";
import {MoreHorizontal} from "lucide-react";
import Link from "next/link";
import {formatLongCoupleName, formatOpenDateRange} from "@/ui/format";
import {toast} from "react-toastify";
import {Dialog, DialogContent} from "@/ui/dialog";
import {EditCoupleForm} from "@/ui/forms/EditCoupleForm";

export function EditCoupleCard({data}: { data: CoupleFragment; }) {
  const auth = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateCoupleDocument)[1];
  const del = useMutation(DeleteCoupleDocument)[1];
  const confirm = useConfirm();

  return (
    <>
      <DropdownMenu key={data.id}>
        <div className="flex gap-3 mb-1 align-baseline">
          {auth.isAdmin && (
            <DropdownMenuTrigger>
              <MoreHorizontal className="size-5 text-neutral-10"/>
            </DropdownMenuTrigger>
          )}

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <Link className="underline font-bold" href={`/pary/${data.id}`}>{formatLongCoupleName(data)}</Link>
            <span>{formatOpenDateRange(data)}</span>
          </div>
        </div>

        <DropdownMenuContent align="start">
          {auth.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit partnerství</DropdownMenuButton>
          )}
          {auth.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({description: `Opravdu chcete pár ${formatLongCoupleName(data)} ukončit ke dnešnímu datu?`})
              await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
              toast.success("Ukončeno");
            }}>Ukončit ke dnešnímu datu</DropdownMenuButton>
          )}
          {auth.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({description: `Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.`})
              await del({id: data.id});
              toast.success("Ukončeno");
            }}>Smazat</DropdownMenuButton>
          )}
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditCoupleForm id={data.id} onSuccess={() => setEditOpen(false)}/>
        </DialogContent>
      </Dialog>
    </>
  );
}
