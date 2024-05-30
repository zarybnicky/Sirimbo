import {
  DeleteEventInstanceDocument,
  EventInstanceWithEventFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { CheckSquare, NotebookPen, Pencil, Square, Trash2 } from 'lucide-react';
import { useConfirm } from '@/ui/Confirm';
import { useMutation } from 'urql';
import React from 'react';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

export function EventInstanceMenu({
  data,
  ...props
}: { data: EventInstanceWithEventFragment } & DropdownMenuContentProps) {
  const confirm = useConfirm();
  const updateInstance = useMutation(UpdateEventInstanceDocument)[1];
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];

  const markCancelled = React.useCallback(() => {
    updateInstance({ id: data.id, patch: { isCancelled: !data.isCancelled } });
  }, [updateInstance, data]);

  const deleteInstance = React.useCallback(async () => {
    if ((data.event?.eventInstancesList.length ?? 0) < 2) {
      await confirm({
        description:
          'Opravdu chcete smazat CELOU UDÁLOST? Smažete tím všechny záznamy o účasti i platbách.',
      });
    } else {
      await confirm({
        description:
          'Opravdu chcete smazat JEDEN TERMÍN události? Smažete tím všechny záznamy o účasti i platbách.',
      });
    }
    await deleteMutation({ id: data.id });
  }, [confirm, data, deleteMutation]);

  return (
    <DropdownMenuContent {...props}>
      <Dialog key="edit">
        <DialogTrigger.Dropdown>
          <Pencil className="size-4" />
          Upravit
        </DialogTrigger.Dropdown>
        <DialogContent>
          <UpsertEventForm event={data.event!} />
        </DialogContent>
      </Dialog>

      <Dialog key="editlong">
        <DialogTrigger.Dropdown>
          <NotebookPen className="size-4" />
          Upravit dlouhý popis
        </DialogTrigger.Dropdown>
        <DialogContent>
          <EditEventDescriptionForm event={data.event!} />
        </DialogContent>
      </Dialog>

      <DropdownMenuButton onClick={markCancelled}>
        {data.isCancelled ? (
          <CheckSquare className="size-4" />
        ) : (
          <Square className="size-4" />
        )}
        {data.isCancelled ? 'Zrušeno' : 'Zrušit termín'}
      </DropdownMenuButton>

      <DropdownMenuButton onClick={deleteInstance}>
        <Trash2 className="size-4" />
        Odstranit termín
      </DropdownMenuButton>

      <DropdownMenuButton onClick={() => exportEventParticipants(data.event!.id)}>
        Export přihlášených
      </DropdownMenuButton>

      <DropdownMenuButton onClick={() => exportEventRegistrations(data.event!.id)}>
        Export přihlášek
      </DropdownMenuButton>
    </DropdownMenuContent>
  );
}
