import { EventFragment } from '@/graphql/Event';
import { NotebookPen, Pencil } from 'lucide-react';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import React from 'react';
import { UpsertEventForm } from './event-form/UpsertEventForm';
import { EditEventDescriptionForm } from './forms/EditEventDescriptionForm';
import { DropdownMenuButton, DropdownMenuContent } from './dropdown';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { exportEventRegistrations } from './reports/export-event-registrations';
import { exportEventParticipants } from './reports/export-event-participants';

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
