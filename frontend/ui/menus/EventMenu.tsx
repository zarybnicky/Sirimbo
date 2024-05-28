import { EventFragment } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { NotebookPen, Pencil } from 'lucide-react';
import React from 'react';

export function EventMenu({
  data,
  ...props
}: { data: EventFragment } & DropdownMenuContentProps) {
  return (
    <DropdownMenuContent {...props}>
      <Dialog modal={false}>
        <DialogTrigger.Dropdown>
          <Pencil className="size-4" />
          Upravit
        </DialogTrigger.Dropdown>
        <DialogContent>
          <UpsertEventForm event={data} />
        </DialogContent>
      </Dialog>

      <Dialog key="editlong">
        <DialogTrigger.Dropdown>
          <NotebookPen className="size-4" />
          Upravit dlouhý popis
        </DialogTrigger.Dropdown>
        <DialogContent>
          <EditEventDescriptionForm event={data} />
        </DialogContent>
      </Dialog>

      <DropdownMenuButton onClick={() => exportEventParticipants(data.id)}>
        Export přihlášených
      </DropdownMenuButton>

      <DropdownMenuButton onClick={() => exportEventRegistrations(data.id)}>
        Export přihlášek
      </DropdownMenuButton>
    </DropdownMenuContent>
  );
}
