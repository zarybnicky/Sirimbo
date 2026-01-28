import {
  DeleteEventInstanceDocument,
  DetachEventInstanceDocument,
  EventDocument,
  EventFragment,
  EventInstanceWithTrainerFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent } from '@/ui/dropdown';
import {
  CheckSquare,
  GitBranch,
  NotebookPen,
  Pencil,
  Square,
  Trash2,
} from 'lucide-react';
import { useConfirm } from '@/ui/Confirm';
import { useClient, useMutation } from 'urql';
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
}: {
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
} & DropdownMenuContentProps) {
  const confirm = useConfirm();
  const client = useClient();
  const doDetachInstance = useMutation(DetachEventInstanceDocument)[1];
  const updateInstance = useMutation(UpdateEventInstanceDocument)[1];
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];
  const auth = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);
  const [longEditOpen, setLongEditOpen] = React.useState(false);

  const markCancelled = React.useCallback(() => {
    updateInstance({ id: instance.id, patch: { isCancelled: !instance.isCancelled } });
  }, [updateInstance, instance]);

  const detachInstance = React.useCallback(async () => {
    try {
      await confirm({
        description: [
          'Opravdu chcete oddělit tento termín do samostatné události?',
          'Termín bude odebrán z původní události a vytvoří se nová událost se stejným nastavením.',
        ].join(' '),
        confirmationText: 'Oddělit termín',
      });
    } catch {
      return;
    }

    const result = await doDetachInstance({ id: instance.id });
    if (result.error) {
      throw result.error;
    }

    await client.query(EventDocument, { id: event.id }).toPromise();

    const newEventId = result.data?.detachEventInstance?.event?.id;
    if (newEventId)
      await client.query(EventDocument, { id: newEventId }).toPromise();
  }, [client, confirm, event.id, instance.id, doDetachInstance]);

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

  if (
    !auth.isAdmin &&
    (!auth.isTrainer ||
      !(instance.trainersList || []).some((x) => auth.personIds.includes(x.personId)))
  )
    return null;

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

        <DropdownMenuButton onSelect={detachInstance}>
          <GitBranch className="size-4" />
          Oddělit termín
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
