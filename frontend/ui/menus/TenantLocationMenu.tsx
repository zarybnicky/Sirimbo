import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { Pencil } from 'lucide-react';
import React from 'react';
import { EditTenantLocationForm } from '../forms/EditLocationForm';
import { useAuth } from '@/ui/use-auth';

export function TenantLocationMenu({
  id,
  children,
  ...props
}: { id: string } & DropdownMenuContentProps) {
  const auth = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);

  if (!auth.isAdmin) {
    return null;
  }

  return (
    <DropdownMenu>
      {children}

      <DropdownMenuContent {...props}>
        <DropdownMenuButton onSelect={() => setEditOpen(true)}>
          <Pencil className="size-4" />
          Upravit
        </DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantLocationForm id={id} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
