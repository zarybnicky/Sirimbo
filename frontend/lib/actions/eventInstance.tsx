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
import { Action, type ActionContext } from '@/lib/actions';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export const eventInstanceActions: Action<EventInstanceWithTrainerFragment>[] = [
  {
    id: 'eventInstance.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'dialog',
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
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'dialog',
    render: ({ item }) => <EditEventDescriptionForm id={item.eventId} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'eventInstance.toggleCancelled',
    label: ({ item }) => (item.isCancelled ? 'Zrušeno' : 'Zrušit termín'),
    icon: (ctx: ActionContext<EventInstanceWithTrainerFragment>) =>
      ctx.item.isCancelled ? CheckSquare : Square,
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'mutation',
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
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'mutation',
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
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'mutation',
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
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'mutation',
    execute: async ({ item, client }) => {
      await exportEventParticipants(client, item.eventId);
    },
  },
  {
    id: 'eventInstance.exportRegistrations',
    label: 'Export přihlášek',
    visible: ({ auth, item }) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        (item.trainersList || []).some((x) => auth.isMyPerson(x.personId))),
    type: 'mutation',
    execute: async ({ item, client }) => {
      await exportEventRegistrations(client, item.eventId);
    },
  },
];
