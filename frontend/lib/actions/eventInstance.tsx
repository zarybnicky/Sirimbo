import {
  CalendarPlus,
  CheckSquare,
  GitBranch,
  NotebookPen,
  Pencil,
  Share2,
  Square,
  Trash2,
  UserPlus,
} from 'lucide-react';
import {
  DeleteEventDocument,
  type EventRegistrationFragment,
  type EventWithTrainerFragment,
  UpdateEventDocument,
} from '@/graphql/Event';
import { type ActionContext, defineActions } from '@/lib/actions';
import { EditEventForm } from '@/ui/event-form/EventForms';
import { AddToEventScheduleForm } from '@/ui/forms/AddToEventScheduleForm';
import { EditEventDescriptionForm } from '@/ui/forms/EditEventDescriptionForm.tsx';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export const eventRegistrationActions = defineActions<EventRegistrationFragment & {
  instance: EventWithTrainerFragment;
}>()([
  {
    id: 'eventRegistration.edit',
    label: 'Upravit přihlášku',
    icon: Pencil,
    visible: ({ auth, item }) =>
      auth.canManageInstance(item.instance) ||
      auth.isMyPerson(item.personId) || auth.isMyCouple(item.coupleId),
    render: ({ item }) => (
      <MyRegistrationsDialog
        instanceId={item.instance.id}
        initialRegistrationId={item.id}
      />
    ),
  },
]);

function canOpenRegistrations({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  return (
    auth.canManageInstance(item) ||
    !!item.registrationInfo?.my ||
    (!!(item.isPublic || item.isVisible) &&
      !item.isLocked &&
      (auth.personIds.length > 0 ||
        !item.capacity ||
        (item.registrationInfo?.remainingCapacity ?? 0) > 0))
  );
}

function registrationActionLabel({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  if (auth.canManageInstance(item)) {
    return `Přihlášky (${item.registrationInfo?.registrations ?? 0})`;
  }

  return !auth.isLoggedIn || !item.registrationInfo?.my ? 'Přihlásit' : 'Moje přihlášky';
}

export const eventInstanceActions = defineActions<EventWithTrainerFragment>()([
  {
    id: 'eventInstance.registrations',
    group: 'primary',
    label: registrationActionLabel,
    icon: UserPlus,
    visible: canOpenRegistrations,
    render: ({ item }) => <MyRegistrationsDialog instanceId={item.id} />,
  },
  {
    id: 'eventInstance.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.canManageInstance(item),
    render: ({ item }) => <EditEventForm event={item} />,
    dialogProps: {
      className: 'sm:max-w-xl',
      onOpenAutoFocus: preventDefault,
    },
  },
  {
    id: 'eventInstance.editDescription',
    label: 'Upravit dlouhý popis',
    icon: NotebookPen,
    visible: ({ auth, item }) => auth.canManageInstance(item),
    render: ({ item }) => <EditEventDescriptionForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'eventInstance.share',
    label: 'Sdílet program',
    icon: Share2,
    visible: ({ auth, item }) => auth.canManageInstance(item),
    load: () => import('@/ui/EventShareDialog'),
  },
  {
    id: 'eventInstance.toggleCancelled',
    label: ({ item }) => (item.isCancelled ? 'Zrušeno' : 'Zrušit termín'),
    icon: ({ item }: ActionContext<EventWithTrainerFragment>) =>
      item.isCancelled ? CheckSquare : Square,
    visible: ({ auth, item }) => auth.canManageInstance(item),
    execute: async ({ item, mutate }) => {
      await mutate(UpdateEventDocument, {
        id: item.id,
        patch: { isCancelled: !item.isCancelled },
      });
    },
  },
  {
    id: 'eventInstance.attendance',
    label: 'Docházka',
    visible: ({ auth, item }) => auth.canManageInstance(item),
    href: ({ item }) => `/termin/${item.id}?tab=registrations`,
  },
  {
    id: 'eventInstance.addToEventSchedule',
    label: 'Přidat do rozpisu události',
    icon: CalendarPlus,
    visible: ({ item, auth }) => auth.canManageInstance(item) && !item.parentId,
    render: ({ item }) => <AddToEventScheduleForm eventId={item.id} />,
  },
  {
    id: 'eventInstance.detach',
    label: ({ item }) => (item.parentId ? 'Vyjmout z programu' : 'Oddělit termín'),
    icon: GitBranch,
    visible: ({ item, auth }) =>
      auth.canManageInstance(item) && !!(item.parentId || item.seriesId),
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
        await mutate(UpdateEventDocument, {
          id: item.id,
          patch: { parentId: null },
        });
      } else {
        await mutate(UpdateEventDocument, {
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
    visible: ({ auth, item }) => auth.canManageInstance(item),
    confirm: {
      description:
        'Opravdu chcete smazat termín? Tím se smažou také všechny jeho záznamy o účasti a platby.',
    },
    execute: async ({ item, mutate }) => {
      await mutate(DeleteEventDocument, { id: item.id });
    },
  },
  {
    id: 'eventInstance.exportParticipants',
    label: 'Export přihlášených',
    visible: ({ auth, item }) => auth.canManageInstance(item),
    execute: async ({ item, client }) => {
      await exportEventParticipants(client, item.id);
    },
  },
  {
    id: 'eventInstance.exportRegistrations',
    label: 'Export přihlášek',
    visible: ({ auth, item }) => auth.canManageInstance(item),
    execute: async ({ item, client }) => {
      await exportEventRegistrations(client, item.id);
    },
  },
]);
