import { NotebookPen, Pencil, Trash2, X } from 'lucide-react';
import type {
  EventFragment,
  EventFullFragment,
} from '@/graphql/Event';
import {
  CancelRegistrationDocument,
  DeleteEventExternalRegistrationDocument,
} from '@/graphql/Event';
import { Action } from '@/lib/actions';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { MyRegistrationForm } from '@/ui/forms/MyRegistrationForm';
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
      (auth.isTrainer &&
        item.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
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
      (auth.isTrainer &&
        item.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
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
      (auth.isTrainer &&
        item.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
    type: 'mutation',
    execute: async ({ item }) => {
      exportEventParticipants(item.id);
    },
  },
  {
    id: 'event.exportRegistrations',
    label: 'Export přihlášek',
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        item.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
    type: 'mutation',
    execute: async ({ item }) => {
      exportEventRegistrations(item.id);
    },
  },
];

export type EventRegistrationActionItem = {
  id: string;
  event: EventFullFragment;
  registration: EventFullFragment['eventRegistrationsList'][number];
};

export type EventExternalRegistrationActionItem = {
  id: string;
  event: EventFullFragment;
  registration: EventFullFragment['eventExternalRegistrationsList'][number];
};

export const eventRegistrationActions: Action<EventRegistrationActionItem>[] = [
  {
    id: 'eventRegistration.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        item.event.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
    type: 'dialog',
    render: ({ item }) => (
      <MyRegistrationForm registration={item.registration} event={item.event} />
    ),
    dialogProps: {
      className: 'sm:max-w-xl',
      onOpenAutoFocus: preventDefault,
    },
  },
  {
    id: 'eventRegistration.cancel',
    label: 'Zrušit přihlášku',
    icon: X,
    variant: 'danger',
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        item.event.eventTrainersList.some((x) => auth.personIds.includes(x.personId))),
    type: 'mutation',
    confirm: 'Opravdu chcete zrušit přihlášku?',
    execute: async ({ item, mutate }) => {
      await mutate(CancelRegistrationDocument, {
        input: { registrationId: item.registration.id },
      });
    },
  },
];

export const eventExternalRegistrationActions: Action<EventExternalRegistrationActionItem>[] = [
  {
    id: 'eventExternalRegistration.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: 'Opravdu chcete smazat externí přihlášku?',
    execute: async ({ item, mutate }) => {
      await mutate(DeleteEventExternalRegistrationDocument, { id: item.registration.id });
    },
  },
];
