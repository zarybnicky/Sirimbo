import { NotebookPen, Pencil, Trash2 } from 'lucide-react';
import type { EventFragment } from '@/graphql/Event';
import { DeleteEventExternalRegistrationDocument } from '@/graphql/Event';
import { defineActions } from '@/lib/actions';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export const eventActions = defineActions<EventFragment>()([
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
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer && item.eventTrainersList.some((x) => auth.isMyPerson(x.personId))),
    render: ({ item }) => <EditEventDescriptionForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'event.exportParticipants',
    label: 'Export přihlášených',
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer && item.eventTrainersList.some((x) => auth.isMyPerson(x.personId))),
    execute: async ({ item, client }) => {
      await exportEventParticipants(client, item.id);
    },
  },
  {
    id: 'event.exportRegistrations',
    label: 'Export přihlášek',
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer && item.eventTrainersList.some((x) => auth.isMyPerson(x.personId))),
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
