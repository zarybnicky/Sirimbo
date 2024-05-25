import { TenantLocationFragment } from "@/graphql/Tenant";
import { Dialog, DialogContent, DialogTrigger } from "@/ui/dialog";
import { DropdownMenu, DropdownMenuContent, DropdownMenuTrigger } from "@/ui/dropdown";
import { EditTenantLocationForm } from "@/ui/forms/EditLocationForm";
import { useAuth } from "@/ui/use-auth";
import { MoreHorizontal } from "lucide-react";
import React from "react";

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
              <DialogTrigger.Dropdown text="Upravit místo" />
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
