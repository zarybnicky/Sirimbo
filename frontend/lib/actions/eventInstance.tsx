import {
  CheckSquare,
  GitBranch,
  NotebookPen,
  Pencil,
  Square,
  Trash2,
} from 'lucide-react';
import {
  DeleteEventInstanceDocument,
  DetachEventInstanceDocument,
  EventDocument,
  type EventInstanceWithTrainerFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { type ActionContext, defineActions } from '@/lib/actions';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export function canManageInstance({
  auth,
  item,
}: Pick<ActionContext<EventInstanceWithTrainerFragment>, 'auth' | 'item'>) {
  return (
    auth.isAdmin ||
    (auth.isTrainer &&
      auth.personIds.some((personId) => item.managerPersonIds.includes(personId)))
  );
}

export const eventInstanceActions = defineActions<EventInstanceWithTrainerFragment>()([
  {
    id: 'eventInstance.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: canManageInstance,
    render: ({ item }) => <UpsertEventForm eventId={item.eventId} />,
    dialogProps: {
      className: 'sm:max-w-xl',
      onOpenAutoFocus: preventDefault,
    },
  },
  {
    id: 'eventInstance.editDescription',
    label: 'Upravit dlouhý popis',
    icon: NotebookPen,
    visible: canManageInstance,
    render: ({ item }) => <EditEventDescriptionForm id={item.eventId} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'eventInstance.toggleCancelled',
    label: ({ item }) => (item.isCancelled ? 'Zrušeno' : 'Zrušit termín'),
    icon: ({ item }: ActionContext<EventInstanceWithTrainerFragment>) =>
      item.isCancelled ? CheckSquare : Square,
    visible: canManageInstance,
    execute: async ({ item, mutate }) => {
      await mutate(UpdateEventInstanceDocument, {
        id: item.id,
        patch: { isCancelled: !item.isCancelled },
      });
    },
  },
  {
    id: 'eventInstance.detach',
    label: 'Oddělit termín',
    icon: GitBranch,
    visible: canManageInstance,
    confirm: {
      description: [
        'Opravdu chcete oddělit tento termín do samostatné události?',
        'Termín bude odebrán z původní události a vytvoří se nová událost se stejným nastavením.',
      ].join(' '),
      confirmationText: 'Oddělit termín',
    },
    execute: async ({ item, client, mutate }) => {
      const result = await mutate(DetachEventInstanceDocument, { id: item.id });

      await client.query(EventDocument, { id: item.eventId }).toPromise();

      const newEventId = result.detachEventInstance?.event?.id;
      if (newEventId) {
        await client.query(EventDocument, { id: newEventId }).toPromise();
      }
    },
  },
  {
    id: 'eventInstance.delete',
    label: 'Odstranit termín',
    icon: Trash2,
    variant: 'danger',
    visible: canManageInstance,
    confirm: {
      description:
        'Opravdu chcete smazat termín? Pokud to je poslední termín, smažete tím i celou událost. Tím se taky smažou všechny záznamy o účasti i platbách.',
    },
    execute: async ({ item, mutate }) => {
      await mutate(DeleteEventInstanceDocument, { id: item.id });
    },
  },
  {
    id: 'eventInstance.exportParticipants',
    label: 'Export přihlášených',
    visible: canManageInstance,
    execute: async ({ item, client }) => {
      await exportEventParticipants(client, item.eventId);
    },
  },
  {
    id: 'eventInstance.exportRegistrations',
    label: 'Export přihlášek',
    visible: canManageInstance,
    execute: async ({ item, client }) => {
      await exportEventRegistrations(client, item.eventId);
    },
  },
]);
