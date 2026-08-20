import {
  CalendarPlus,
  CheckSquare,
  GitBranch,
  NotebookPen,
  Pencil,
  Share2,
  Square,
  Trash2,
} from 'lucide-react';
import {
  DeleteEventDocument,
  DeleteEventExternalRegistrationDocument,
  type EventWithTrainerFragment,
  UpdateEventDocument,
} from '@/graphql/Event';
import { type ActionContext, defineActions } from '@/lib/actions';
import { EditEventForm } from '@/ui/event-form/EventForms';
import { AddToEventScheduleForm } from '@/ui/forms/AddToEventScheduleForm';
import { EditEventInstanceDescriptionForm } from '@/ui/forms/EditEventInstanceDescriptionForm';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { exportEventParticipants } from '@/ui/reports/export-event-participants';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';

const preventDefault = (e: Event) => e.preventDefault();

export function canManageInstance({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  return (
    auth.isAdmin ||
    (auth.isTrainer && auth.personIds.some((x) => item.managerPersonIds.includes(x)))
  );
}

function canOpenRegistrations({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  return (
    canManageInstance({ auth, item }) ||
    (!!(item.isPublic || item.isVisible) &&
      (!auth.isExternal ||
        !item.capacity ||
        (item.registrationInfo?.remainingCapacity ?? 0) > 0))
  );
}

function registrationActionLabel({
  auth,
  item,
}: Pick<ActionContext<EventWithTrainerFragment>, 'auth' | 'item'>) {
  if (canManageInstance({ auth, item })) {
    return `Přihlášky (${item.registrationInfo?.registrations ?? 0})`;
  }

  return !auth.isLoggedIn || !item.registrationInfo?.my
    ? 'Přihlásit'
    : 'Moje přihlášky';
}

export const eventInstanceActions = defineActions<EventWithTrainerFragment>()([
  {
    id: 'eventInstance.registrations',
    group: 'primary',
    label: registrationActionLabel,
    visible: canOpenRegistrations,
    render: ({ auth, item }) => (
      <MyRegistrationsDialog
        instance={item}
        isManager={canManageInstance({ auth, item })}
      />
    ),
  },
  {
    id: 'eventInstance.edit',
    group: 'primary',
    label: 'Upravit',
    icon: Pencil,
    visible: canManageInstance,
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
    visible: canManageInstance,
    render: ({ item }) => <EditEventInstanceDescriptionForm id={item.id} />,
    dialogProps: {
      className: 'sm:max-w-xl',
    },
  },
  {
    id: 'eventInstance.share',
    label: 'Sdílet program',
    icon: Share2,
    visible: canManageInstance,
    load: () => import('@/ui/EventShareDialog'),
  },
  {
    id: 'eventInstance.toggleCancelled',
    label: ({ item }) => (item.isCancelled ? 'Zrušeno' : 'Zrušit termín'),
    icon: ({ item }: ActionContext<EventWithTrainerFragment>) =>
      item.isCancelled ? CheckSquare : Square,
    visible: canManageInstance,
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
    visible: canManageInstance,
    href: ({ item }) => `/termin/${item.id}?tab=attendance`,
  },
  {
    id: 'eventInstance.addToEventSchedule',
    label: 'Přidat do rozpisu události',
    icon: CalendarPlus,
    visible: ({ item, auth }) => canManageInstance({ item, auth }) && !item.parentId,
    render: ({ item }) => <AddToEventScheduleForm eventId={item.id} />,
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
    visible: canManageInstance,
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
