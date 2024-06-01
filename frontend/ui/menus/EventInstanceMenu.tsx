import {
  DeleteEventInstanceDocument,
  EventInstanceWithEventFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { CheckSquare, NotebookPen, Pencil, Square, Trash2 } from 'lucide-react';
import { useConfirm } from '@/ui/Confirm';
import { useMutation } from 'urql';
import React from 'react';
import { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { Dialog, DialogContent } from '@/ui/dialog';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';
import { useAuth } from '../use-auth';

export function EventInstanceMenu({
  data,
  children,
  ...props
}: { data: EventInstanceWithEventFragment } & DropdownMenuContentProps) {
  const confirm = useConfirm();
  const updateInstance = useMutation(UpdateEventInstanceDocument)[1];
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);
  const [longEditOpen, setLongEditOpen] = React.useState(false);

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

        <DropdownMenuButton onSelect={markCancelled}>
          {data.isCancelled ? (
            <CheckSquare className="size-4" />
          ) : (
            <Square className="size-4" />
          )}
          {data.isCancelled ? 'Zrušeno' : 'Zrušit termín'}
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={deleteInstance}>
          <Trash2 className="size-4" />
          Odstranit termín
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventParticipants(data.event!.id)}>
          Export přihlášených
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventRegistrations(data.event!.id)}>
          Export přihlášek
        </DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent className="sm:max-w-xl">
          <UpsertEventForm event={data.event!} />
        </DialogContent>
      </Dialog>

      <Dialog open={longEditOpen} onOpenChange={setLongEditOpen}>
        <DialogContent className="sm:max-w-xl">
          <EditEventDescriptionForm event={data.event!} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
