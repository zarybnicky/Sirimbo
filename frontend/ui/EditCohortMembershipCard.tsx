import {
  CohortMembershipFragment,
  DeleteCohortMembershipDocument,
  UpdateCohortMembershipDocument
} from "@/graphql/Memberships";
import {useAuth} from "@/ui/use-auth";
import React from "react";
import {useMutation} from "urql";
import {useConfirm} from "@/ui/Confirm";
import {toast} from "react-toastify";
import {DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger} from "@/ui/dropdown";
import {MoreHorizontal} from "lucide-react";
import Link from "next/link";
import {formatOpenDateRange} from "@/ui/format";
import {Dialog, DialogContent} from "@/ui/dialog";
import {EditCohortMembershipForm} from "@/ui/forms/EditCohortMembershipForm";

export function EditCohortMembershipCard({data, showPerson}: {
  data: CohortMembershipFragment;
  showPerson?: boolean;
}) {
  const auth = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const del = useMutation(DeleteCohortMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?`})
    await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Členství ukončeno");
  }, [update, confirm, data]);

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
            {showPerson ? (
              <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>{data.person?.name}</Link>
            ) : (
              <b>
                Člen skupiny{' '}
                <Link className="underline font-bold"
                      href={`/treninkove-skupiny/${data.cohort?.id}`}>{data.cohort?.name}</Link>
              </b>
            )}
            <span>{formatOpenDateRange(data)}</span>
          </div>
        </div>

        <DropdownMenuContent align="start">
          {auth.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit členství</DropdownMenuButton>
          )}
          {auth.isAdmin && (
            <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
          )}
          {auth.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({description: `Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.`})
              await del({id: data.id});
              toast.success("Odstraněno");
            }}>Smazat</DropdownMenuButton>
          )}
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditCohortMembershipForm id={data.id} onSuccess={() => setEditOpen(false)}/>
        </DialogContent>
      </Dialog>
    </>
  );
}
