import { LogInAsDocument } from "@/graphql/CurrentUser";
import { DeleteUserProxyDocument, UpdateUserProxyDocument, UserProxyFragment } from "@/graphql/Memberships";
import { useConfirm } from "@/ui/Confirm";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenuButton, DropdownMenuContent } from "@/ui/dropdown";
import { EditUserProxyForm } from "@/ui/forms/EditUserProxyForm";
import { useRouter } from "next/router";
import React from "react";
import { toast } from "react-toastify";
import { useMutation } from "urql";

export function UserProxyMenu({ data }: { data: UserProxyFragment }) {
  const update = useMutation(UpdateUserProxyDocument)[1];
  const doRemove = useMutation(DeleteUserProxyDocument)[1];
  const doLogInAs = useMutation(LogInAsDocument)[1];
  const confirm = useConfirm();
  const router = useRouter();

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

  const logInAs = React.useCallback(async () => {
    await doLogInAs({ id: data.user?.id! });
    router.replace('/dashboard')
  }, [data, router, doLogInAs]);

  return (
    <DropdownMenuContent align="start">
      <Dialog>
        <DialogTrigger.Dropdown text="Upravit platnost" />
        <DialogContent>
          <EditUserProxyForm id={data.id} />
        </DialogContent>
      </Dialog>
      <DropdownMenuButton onClick={logInAs}>Přihlásit se jako...</DropdownMenuButton>
      <DropdownMenuButton onClick={endToday}>Ukončit ke dnešnímu datu</DropdownMenuButton>
      <DropdownMenuButton onClick={remove}>Smazat</DropdownMenuButton>
    </DropdownMenuContent>
  );
}
