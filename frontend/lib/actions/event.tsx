import { NotebookPen, Pencil, Trash2 } from 'lucide-react';
import type { EventFragment } from '@/graphql/Event';
import { DeleteEventExternalRegistrationDocument } from '@/graphql/Event';
import { Action } from '@/lib/actions';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export const eventActions: Action<EventFragment>[] = [
  {
    id: 'event.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer && item.eventTrainersList.some((x) => auth.isMyPerson(x.personId))),
    type: 'dialog',
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
    type: 'dialog',
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
    type: 'mutation',
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
    type: 'mutation',
    execute: async ({ item, client }) => {
      await exportEventRegistrations(client, item.id);
    },
  },
];

export const eventExternalRegistrationActions: Action<{ id: string }>[] = [
  {
    id: 'eventExternalRegistration.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteEventExternalRegistrationDocument, { id: item.id });
    },
  },
];
