import {TenantLocationFragment} from "@/graphql/Tenant";
import {useAuth} from "@/ui/use-auth";
import React from "react";
import {DropdownMenu, DropdownMenuContent, DropdownMenuTrigger} from "@/ui/dropdown";
import {MoreHorizontal} from "lucide-react";
import {Dialog, DialogContent, StdDialogTrigger} from "@/ui/dialog";
import {EditTenantLocationForm} from "@/ui/forms/EditLocationForm";

export function EditTenantLocationCard({data}: { data: TenantLocationFragment; }) {
  const auth = useAuth();

  return (
    <div className="flex gap-3 mb-1">
      {auth.isAdmin && (
        <DropdownMenu key={data.id}>
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10"/>
          </DropdownMenuTrigger>

          <DropdownMenuContent align="start">
            <Dialog>
              <StdDialogTrigger.Dropdown text="Upravit místo" />
              <DialogContent>
                <EditTenantLocationForm id={data.id} />
              </DialogContent>
            </Dialog>
          </DropdownMenuContent>
        </DropdownMenu>
      )}

      <div className="grow gap-2 flex text-sm py-1">
        <b>{data.name}</b>
      </div>
    </div>
  );
}
