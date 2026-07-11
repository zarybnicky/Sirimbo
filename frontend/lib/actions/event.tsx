import { NotebookPen, Pencil, Trash2 } from 'lucide-react';
import type { EventFullFragment } from '@/graphql/Event';
import { DeleteEventExternalRegistrationDocument } from '@/graphql/Event';
import { defineActions } from '@/lib/actions';
import { canManageInstance } from '@/lib/actions/eventInstance';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventInstanceDescriptionForm } from '@/ui/forms/EditEventInstanceDescriptionForm';

const preventDefault = (e: Event) => e.preventDefault();
const rootInstance = (event: EventFullFragment) =>
  event.eventInstancesList.find((instance) => !instance.parentId) ??
  event.eventInstancesList[0];
const rootInstanceId = (event: EventFullFragment) => rootInstance(event)?.id;

export const eventActions = defineActions<EventFullFragment>()([
  {
    id: 'event.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer && item.eventTrainersList.some((x) => auth.isMyPerson(x.personId))),
    render: ({ item }) => <UpsertEventForm eventId={item.id} />,
    dialogProps: {
      className: 'sm:max-w-xl',
      onOpenAutoFocus: preventDefault,
    },
  },
  {
    id: 'event.editDescription',
    label: 'Upravit dlouhý popis',
    icon: NotebookPen,
    visible: ({ auth, item }) => {
      const instance = rootInstance(item);
      return !!instance && canManageInstance({ auth, item: instance });
    },
    render: ({ item }) => <EditEventInstanceDescriptionForm id={rootInstanceId(item)!} />,
    modal: false,
    dialogProps: {
      className: 'sm:max-w-xl',
      onPointerDownOutside: (e) => e.preventDefault(),
      onInteractOutside: (e) => e.preventDefault(),
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
