import { DeleteUserProxyDocument, UpdateUserProxyDocument, UserProxyFragment } from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { formatOpenDateRange } from "@/ui/format";
import { EditUserProxyForm } from "@/ui/forms/EditUserProxyForm";
import { useAuth } from "@/ui/use-auth";
import { MoreHorizontal } from "lucide-react";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function EditUserProxyCard({ data }: { data: UserProxyFragment }) {
  const auth = useAuth();
  const update = useMutation(UpdateUserProxyDocument)[1];
  const doRemove = useMutation(DeleteUserProxyDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete ukončit platnost těchto přihlašovacích údajů?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } } });
    toast.success("Ukončeno");
  }, [confirm, data.id, update]);

  const remove = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?` })
    await doRemove({ id: data.id });
    toast.success("Ukončeno");
  }, [confirm, data.id, doRemove]);

  return (
    <div className="flex gap-3 mb-1 align-baseline">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10" />
          </DropdownMenuTrigger>

          <DropdownMenuContent align="start">
            <Dialog>
              <DialogTrigger.Dropdown text="Upravit platnost" />
              <DialogContent>
                <EditUserProxyForm id={data.id} />
              </DialogContent>
            </Dialog>
            <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
            <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
          </DropdownMenuContent>
        </DropdownMenu>
      )}

      <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
        <b>{data.user?.uEmail}, {data.user?.uLogin}</b>
        <span>{formatOpenDateRange(data)}</span>
      </div>
    </div>
  );
}
