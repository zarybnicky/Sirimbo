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
  DeleteEventExternalRegistrationDocument,
  type EventWithTrainerFragment,
  UpdateEventInstanceDocument,
} from '@/graphql/Event';
import { type ActionContext, defineActions } from '@/lib/actions';
import { EventEditForm } from '@/ui/event-form/EventForms';
import { EditEventInstanceDescriptionForm } from '@/ui/forms/EditEventInstanceDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export function canManageInstance({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  return (
    auth.isAdmin ||
    (auth.isTrainer &&
      auth.personIds.some((personId) => item.managerPersonIds.includes(personId)))
  );
}

export const eventInstanceActions = defineActions<EventWithTrainerFragment>()([
  {
    id: 'eventInstance.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: canManageInstance,
    render: ({ item }) => <EventEditForm instance={item} />,
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
    render: ({ item }) => <EditEventInstanceDescriptionForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'eventInstance.toggleCancelled',
    label: ({ item }) => (item.isCancelled ? 'Zrušeno' : 'Zrušit termín'),
    icon: ({ item }: ActionContext<EventWithTrainerFragment>) =>
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
    id: 'eventInstance.attendance',
    label: 'Docházka',
    visible: canManageInstance,
    href: ({ item }) => `/termin/${item.id}?tab=attendance`,
  },
  {
    id: 'eventInstance.detach',
    label: ({ item }) => (item.parentId ? 'Vyjmout z programu' : 'Oddělit termín'),
    icon: GitBranch,
    visible: ({ item, auth }) =>
      canManageInstance({ item, auth }) && !!(item.parentId || item.seriesId),
    confirm: ({ item }) =>
      item.parentId
        ? {
            description: 'Opravdu chcete termín vyjmout z programu?',
            confirmationText: 'Vyjmout termín',
          }
        : {
            description: 'Opravdu chcete tento termín odebrat ze série?',
            confirmationText: 'Oddělit termín',
          },
    execute: async ({ item, mutate }) => {
      if (item.parentId) {
        await mutate(UpdateEventInstanceDocument, {
          id: item.id,
          patch: { parentId: null },
        });
      } else {
        await mutate(UpdateEventInstanceDocument, {
          id: item.id,
          patch: { seriesId: null },
        });
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
        'Opravdu chcete smazat termín? Tím se smažou také všechny jeho záznamy o účasti a platby.',
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
      await exportEventParticipants(client, item.id);
    },
  },
  {
    id: 'eventInstance.exportRegistrations',
    label: 'Export přihlášek',
    visible: canManageInstance,
    execute: async ({ item, client }) => {
      await exportEventRegistrations(client, item.id);
    },
  },
]);

export const eventExternalRegistrationActions = defineActions<{ id: string }>()([
  {
    id: 'eventExternalRegistration.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    execute: async ({ item, mutate }) => {
      await mutate(DeleteEventExternalRegistrationDocument, { id: item.id });
    },
  },
]);
