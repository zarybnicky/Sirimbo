import {TenantLocationFragment} from "@/graphql/Tenant";
import {useAuth} from "@/ui/use-auth";
import React from "react";
import {DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger} from "@/ui/dropdown";
import {MoreHorizontal} from "lucide-react";
import {Dialog, DialogContent} from "@/ui/dialog";
import {EditTenantLocationForm} from "@/ui/forms/EditLocationForm";

export function EditTenantLocationCard({data}: { data: TenantLocationFragment; }) {
  const auth = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);

  return (
    <DropdownMenu key={data.id}>
      <div className="flex gap-3 mb-1">
        {auth.isAdmin && (
          <DropdownMenuTrigger>
            <MoreHorizontal className="size-5 text-neutral-10"/>
          </DropdownMenuTrigger>
        )}

        <div className="grow gap-2 flex text-sm py-1">
          <b>{data.name}</b>
        </div>
      </div>

      <DropdownMenuContent align="start">
        {auth.isAdmin && (
          <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit místo</DropdownMenuButton>
        )}
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantLocationForm id={data.id} onSuccess={() => setEditOpen(false)}/>
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
