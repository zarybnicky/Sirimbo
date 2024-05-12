import {DeleteUserProxyDocument, UpdateUserProxyDocument, UserProxyFragment} from "@/graphql/Memberships";
import {useAuth} from "@/ui/use-auth";
import React from "react";
import {useMutation} from "urql";
import {useConfirm} from "@/ui/Confirm";
import {toast} from "react-toastify";
import {DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger} from "@/ui/dropdown";
import {MoreHorizontal} from "lucide-react";
import {formatOpenDateRange} from "@/ui/format";
import {Dialog, DialogContent} from "@/ui/dialog";
import {EditUserProxyForm} from "@/ui/forms/EditUserProxyForm";

export function EditUserProxyCard({data}: { data: UserProxyFragment; }) {
  const auth = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateUserProxyDocument)[1];
  const del = useMutation(DeleteUserProxyDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({description: `Opravdu chcete ukončit platnost těchto přihlašovacích údajů?`})
    await update({input: {id: data.id, patch: {until: new Date().toISOString()}}});
    toast.success("Ukončeno");
  }, [confirm, data.id, update]);

  if (!auth.isAdmin) {
    return;
  }

  return (
    <DropdownMenu key={data.id}>
      <div className="flex gap-3 mb-1 align-baseline">
        {auth.isAdmin && (
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10"/>
          </DropdownMenuTrigger>
        )}

        <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
          <b>{data.user?.uEmail}, {data.user?.uLogin}</b>
          <span>{formatOpenDateRange(data)}</span>
        </div>
      </div>

      <DropdownMenuContent align="start">
        <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit platnost</DropdownMenuButton>
        <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
        <DropdownMenuButton onClick={async () => {
          await confirm({description: `Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?`})
          await del({id: data.id});
          toast.success("Ukončeno");
        }}>Smazat</DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditUserProxyForm id={data.id} onSuccess={() => setEditOpen(false)}/>
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
