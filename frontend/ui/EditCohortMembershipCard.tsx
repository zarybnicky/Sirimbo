import {
    CohortMembershipFragment,
    DeleteCohortMembershipDocument,
    UpdateCohortMembershipDocument
} from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { formatOpenDateRange } from "@/ui/format";
import { EditCohortMembershipForm } from "@/ui/forms/EditCohortMembershipForm";
import { useAuth } from "@/ui/use-auth";
import Link from "next/link";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function EditCohortMembershipCard({data, showPerson}: {
  data: CohortMembershipFragment;
  showPerson?: boolean;
}) {
  const auth = useAuth();
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const del = useMutation(DeleteCohortMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?`})
    await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Členství ukončeno");
  }, [update, confirm, data]);

  const remove = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.`})
    await del({id: data.id});
    toast.success("Odstraněno");
  }, [confirm, del, data.id]);

  return (
    <div className="flex gap-3 mb-1 align-baseline">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger.RowDots />
          <DropdownMenuContent align="start">
            <Dialog>
              <DialogTrigger.Dropdown text="Upravit členství" />
              <DialogContent>
                <EditCohortMembershipForm id={data.id} />
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
          <b>
            Člen skupiny{' '}
            <Link className="underline font-bold"
              href={`/treninkove-skupiny/${data.cohort?.id}`}>{data.cohort?.name}</Link>
          </b>
        )}
        <span>{formatOpenDateRange(data)}</span>
      </div>
    </div>
  );
}
