import {
  DeleteEventInstanceDocument,
  EventDocument,
  EventFragment,
  EventInstanceWithTrainerFragment,
  UpdateEventInstanceDocument,
  UpsertEventDocument,
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
import { keyIsNonNull } from '../truthyFilter';

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
  const upsertEvent = useMutation(UpsertEventDocument)[1];
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

    const upsertResult = await upsertEvent({
      input: {
        info: {
          name: event.name,
          summary: event.summary,
          description: event.description,
          type: event.type,
          locationId: event.location?.id ?? null,
          locationText: event.locationText || '',
          capacity: event.capacity,
          isVisible: event.isVisible,
          isPublic: event.isPublic,
          isLocked: event.isLocked,
          enableNotes: event.enableNotes,
          paymentType: event.paymentType,
        },
        trainers: event.eventTrainersList.map((x) => ({
          personId: x.personId,
          lessonsOffered: x.lessonsOffered,
        })),
        cohorts: event.eventTargetCohortsList
          .filter(keyIsNonNull('cohort'))
          .map((x) => ({ cohortId: x.cohort.id })),
        registrations: event.eventRegistrations.nodes.map((x) => ({
          coupleId: x.coupleId,
          personId: x.personId,
        })),
        instances: [],
      },
    });
    if (upsertResult.error) {
      throw upsertResult.error;
    }

    const newEventId = upsertResult.data?.upsertEvent?.event?.id;
    if (!newEventId) return;

    const updateResult = await updateInstance({
      id: instance.id,
      patch: { eventId: newEventId },
    });
    if (updateResult.error) {
      throw updateResult.error;
    }

    // Re-assign attendance records

    await client.query(EventDocument, { id: event.id }).toPromise();
    await client.query(EventDocument, { id: newEventId }).toPromise();
  }, [client, confirm, event, instance.id, upsertEvent, updateInstance]);

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
