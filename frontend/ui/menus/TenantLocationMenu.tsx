import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { Pencil } from 'lucide-react';
import React from 'react';
import { EditTenantLocationForm } from '../forms/EditLocationForm';

export function TenantLocationMenu({
  id,
  children,
  ...props
}: { id: string } & DropdownMenuContentProps) {
  const [editOpen, setEditOpen] = React.useState(false);

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
