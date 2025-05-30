import {
  DeleteEventInstanceDocument,
  EventFragment,
  EventInstanceFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import { CheckSquare, NotebookPen, Pencil, Square, Trash2 } from 'lucide-react';
import { useConfirm } from '@/ui/Confirm';
import { useMutation } from 'urql';
import React from 'react';
import type { DropdownMenuContentProps } from '@radix-ui/react-dropdown-menu';
import { Dialog, DialogContent } from '@/ui/dialog';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';
import { useAuth } from '../use-auth';

export function EventInstanceMenu({
  event,
  instance,
  children,
  ...props
}: { event: EventFragment; instance: EventInstanceFragment } & DropdownMenuContentProps) {
  const confirm = useConfirm();
  const updateInstance = useMutation(UpdateEventInstanceDocument)[1];
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);
  const [longEditOpen, setLongEditOpen] = React.useState(false);

  const markCancelled = React.useCallback(() => {
    updateInstance({ id: instance.id, patch: { isCancelled: !instance.isCancelled } });
  }, [updateInstance, instance]);

  const deleteInstance = React.useCallback(async () => {
    if ((event?.eventInstancesList.length ?? 0) < 2) {
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
    await deleteMutation({ id: instance.id });
  }, [event?.eventInstancesList.length, deleteMutation, instance.id, confirm]);

  if (!auth.isAdmin && (!auth.isTrainer || !event.eventTrainersList.some(x => auth.personIds.includes(x.personId)))) return null;

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
          {instance.isCancelled ? (
            <CheckSquare className="size-4" />
          ) : (
            <Square className="size-4" />
          )}
          {instance.isCancelled ? 'Zrušeno' : 'Zrušit termín'}
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={deleteInstance}>
          <Trash2 className="size-4" />
          Odstranit termín
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventParticipants(event.id)}>
          Export přihlášených
        </DropdownMenuButton>

        <DropdownMenuButton onSelect={() => exportEventRegistrations(event.id)}>
          Export přihlášek
        </DropdownMenuButton>
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
          <UpsertEventForm event={event} />
        </DialogContent>
      </Dialog>

      <Dialog open={longEditOpen} onOpenChange={setLongEditOpen}>
        <DialogContent className="sm:max-w-xl">
          <EditEventDescriptionForm event={event} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}

const preventDefault = (e: Event) => e.preventDefault();
