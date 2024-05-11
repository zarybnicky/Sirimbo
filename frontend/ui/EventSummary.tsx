import { DeleteEventInstanceDocument, EventInstanceWithEventFragment, UpdateEventInstanceDocument } from '@/graphql/Event';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { formatDefaultEventName, formatRegistrant, shortTimeFormatter } from '@/ui/format';
import { CheckSquare, Clock, MapPin, MoreHorizontal, Pencil, Square, Trash2, User, Users } from 'lucide-react';
import React from 'react';
import { useMutation } from 'urql';
import { useConfirm } from "./Confirm";
import { MyRegistrationsDialog } from './MyRegistrationsDialog';
import { cn } from "./cn";
import { useAuth } from './use-auth';
import Link from 'next/link';

export function EventSummary({ instance, offsetButtons }: {
  instance: EventInstanceWithEventFragment;
  offsetButtons?: boolean;
}) {
  const auth = useAuth();
  const event = instance.event;
  const updateInstance = useMutation(UpdateEventInstanceDocument)[1];
  const markCancelled = React.useCallback(() => updateInstance({ id: instance.id, patch: { isCancelled: !instance.isCancelled } }), [updateInstance, instance]);
  const [editOpen, setEditOpen] = React.useState(false);

  const confirm = useConfirm();
  const deleteMutation = useMutation(DeleteEventInstanceDocument)[1];

  const deleteInstance = React.useCallback(async () => {
    if ((instance.event?.eventInstancesList.length ?? 0) < 2) {
      await confirm({ description: 'Opravdu chcete smazat CELOU UDÁLOST? Smažete tím všechny záznamy o účasti i platbách.' });
    } else {
      await confirm({ description: 'Opravdu chcete smazat JEDEN TERMÍN události? Smažete tím všechny záznamy o účasti i platbách.' });
    }
    await deleteMutation({ id: instance.id });
  }, [confirm, instance, deleteMutation]);


  if (!event) return null;

  const registrationCount = event.eventRegistrations.totalCount;
  const myRegistrations = event.myRegistrationsList || [];
  const start = new Date(instance.since);
  const end = new Date(instance.until);

  return (
    <div className="flex flex-col gap-2 text-sm">
      {offsetButtons && (
        <Link href={`/akce/${event.id}`} className="text-xl mt-2">
          {formatDefaultEventName(event)}
        </Link>
      )}

      <div className="flex items-center gap-2">
        <Clock className="size-6 text-accent-11" />
        {shortTimeFormatter.formatRange(start, end)}
      </div>

      {event.location && (
        <div className="flex items-center gap-2">
          <MapPin className="size-6 text-accent-11" />
          {event.location.name}
        </div>
      )}
      {event.locationText && (
        <div className="flex items-center gap-2">
          <MapPin className="size-6 text-accent-11" />
          {event.locationText}
        </div>
      )}

      {event.eventTrainersList.length > 0 && (
        <div className="flex items-center gap-2" key="trainers">
          <User className="size-6 text-accent-11 shrink-0" />
          {event.eventTrainersList.map((x) => x.name).join(', ')}
        </div>
      )}

      <div className="flex items-center gap-2">
        <Users className="size-6 text-accent-11" />
        <span>
          {event.eventTargetCohortsList.length > 0 ? (
            event.eventTargetCohortsList.map(x => (
              <div key={x.id}>{x.cohort?.name}</div>
            ))
          ) : registrationCount  === 0 ? (
            <div>VOLNÁ</div>
          ) : myRegistrations.length > 0 ? (
            myRegistrations.map((reg) => (
              <div key={reg.id}>{formatRegistrant(reg)}</div>
            )).concat(
              registrationCount > myRegistrations.length ? [(
                <div key="more">a dalších {registrationCount - myRegistrations.length} účastníků</div>
              )] : []
            )
          ) : registrationCount < 6 ? (
            event.eventRegistrations.nodes.map(x => (
              <div key={x.id}>{formatRegistrant(x)}</div>
            ))
          ) : (
            `${registrationCount} účastníků`
          )}
        </span>
      </div>

      <MyRegistrationsDialog event={event} />

      {(auth.isAdmin || (auth.isTrainer && event.eventTrainersList.find(x => auth.personIds.some(id => id === x.personId)))) && (
        <>
        <DropdownMenu>
          <DropdownMenuTrigger className={cn("absolute top-4", offsetButtons ? "right-8" : "right-2")}>
            <MoreHorizontal className="size-5 text-neutral-10" />
          </DropdownMenuTrigger>
          <DropdownMenuContent align='end' className="z-[100]">
            <DropdownMenuButton className="inline-flex gap-2" onClick={() => setTimeout(() => setEditOpen(true), 1)}>
              <Pencil className="size-4" />
              Upravit
            </DropdownMenuButton>
            <DropdownMenuButton className="inline-flex gap-2" onClick={markCancelled}>
              {instance.isCancelled ? <CheckSquare className="size-4" /> : <Square className="size-4" />}
              Zrušeno
            </DropdownMenuButton>
            <DropdownMenuButton className="inline-flex gap-2" onClick={deleteInstance}>
              <Trash2 className="size-4" />
              Odstranit
            </DropdownMenuButton>
          </DropdownMenuContent>
        </DropdownMenu>

        <Dialog open={editOpen} onOpenChange={setEditOpen} modal={false}>
          <DialogContent>
            <UpsertEventForm event={event} onSuccess={() => setEditOpen(false)} />
          </DialogContent>
        </Dialog>
        </>
      )}
    </div>
  );
}
