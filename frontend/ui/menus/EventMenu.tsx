import { EventFragment } from '@/graphql/Event';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { NotebookPen, Pencil } from 'lucide-react';
import React from 'react';
import { useAuth } from '../use-auth';

export function EventMenu({
  data,
  children,
  ...props
}: { data: EventFragment } & DropdownMenuContentProps) {
  const [editOpen, setEditOpen] = React.useState(false);
  const [longEditOpen, setLongEditOpen] = React.useState(false);
  const auth = useAuth();

  if (!auth.isAdmin) return null;

  return (
    <DropdownMenu>
      {children}

      <DropdownMenuContent {...props}>
        <DropdownMenuButton onSelect={() => setEditOpen(true)}>
          <Pencil className="size-4" />
          Upravit
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => setLongEditOpen(true)}>
          <NotebookPen className="size-4" />
          Upravit dlouhý popis
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventParticipants(data.id)}>
          Export přihlášených
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventRegistrations(data.id)}>
          Export přihlášek
        </DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent className="sm:max-w-xl">
          <UpsertEventForm event={data} />
        </DialogContent>
      </Dialog>

      <Dialog open={longEditOpen} onOpenChange={setLongEditOpen}>
        <DialogContent className="sm:max-w-xl">
          <EditEventDescriptionForm event={data} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
