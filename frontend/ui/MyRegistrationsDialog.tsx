import {
  EventInstanceRegistrationsDocument,
  type EventInstanceWithTrainerFragment,
} from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { useAuth } from '@/ui/use-auth';
import { NewExternalRegistrationForm } from './forms/NewExternalRegistrationForm';
import { NewInstanceRegistrationForm } from './forms/NewInstanceRegistrationForm';
import { useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { formatRegistrant } from '@/ui/format';

export function MyRegistrationsDialog({
  instance,
}: {
  instance: EventInstanceWithTrainerFragment;
}) {
  const auth = useAuth();
  const isExternalRegistration = !auth.isLoggedIn || auth.personIds.length === 0;

  if (
    instance.isCancelled ||
    instance.isLocked ||
    new Date(instance.until) < new Date() ||
    (!instance.isPublic && !instance.isVisible) ||
    (isExternalRegistration &&
      (instance.capacity ?? 0) > 0 &&
      (instance.remainingPersonSpots ?? 0) <= 0)
  ) {
    return null;
  }

  return (
    <Dialog modal={false}>
      {isExternalRegistration ? (
        <DialogTrigger.Add size="sm" text="Přihlásit" />
      ) : (
        <DialogTrigger size="sm" text="Přihlášky" />
      )}

      <DialogContent>
        <InstanceRegistrationsDialogContent instance={instance} />
      </DialogContent>
    </Dialog>
  );
}

function InstanceRegistrationsDialogContent({
  instance,
}: {
  instance: EventInstanceWithTrainerFragment;
}) {
  const auth = useAuth();
  const [query, refetch] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
    pause: !auth.isLoggedIn || auth.personIds.length === 0,
  });
  const registrations = query.data?.eventInstance?.registrations.nodes ?? [];
  const lessonTrainers = query.data?.eventInstance?.lessonTrainers ?? [];
  const myRegistrations = registrations.filter(
    (r) => (!!r.personId && auth.isMyPerson(r.personId)) || (!!r.coupleId && auth.isMyCouple(r.coupleId)),
  );
  if (!auth.isLoggedIn || auth.personIds.length === 0) {
    return <NewExternalRegistrationForm instanceId={instance.id} />;
  }

  if (!query.data) {
    return query.error ? (
      <FormError error={query.error} />
    ) : (
      <div className="text-sm text-neutral-10">Načítám…</div>
    );
  }

  const occupiedPersonIds = new Set(
    registrations.flatMap((r) =>
      r.personId
        ? [r.personId]
        : [r.couple?.man?.id, r.couple?.woman?.id].filter((id): id is string => !!id),
    ),
  );
  const availablePeople = auth.persons.filter((person) => !occupiedPersonIds.has(person.id));
  const availableCouples = [
    ...new Map(auth.couples.map((couple) => [couple.id, couple])).values(),
  ].filter(
    (couple) =>
      couple.status === 'ACTIVE' &&
      !occupiedPersonIds.has(couple.man?.id ?? '') &&
      !occupiedPersonIds.has(couple.woman?.id ?? ''),
  );
  const remainingCapacity = instance.remainingPersonSpots ?? Number.POSITIVE_INFINITY;

  return (
    <div className="space-y-3">
      {myRegistrations.map((registration) => (
        <MyRegistrationCard
          key={registration.id}
          instance={instance}
          registration={registration}
          lessonTrainers={lessonTrainers}
        />
      ))}

      {(availablePeople.length > 0 || availableCouples.length > 0) &&
        myRegistrations.length > 0 && <div className="text-lg font-bold">Další přihlášky</div>}

      {availablePeople.map((person) => (
        <NewInstanceRegistrationForm
          key={person.id}
          instanceId={instance.id}
          enableNotes={!!instance.enableNotes}
          personId={person.id}
          coupleId={null}
          label={person.name}
          lessonTrainers={lessonTrainers}
          disabled={remainingCapacity < 1}
          onRegistered={() => refetch({ requestPolicy: 'network-only' })}
        />
      ))}

      {availableCouples.map((couple) => (
        <NewInstanceRegistrationForm
          key={couple.id}
          instanceId={instance.id}
          enableNotes={!!instance.enableNotes}
          personId={null}
          coupleId={couple.id}
          label={formatRegistrant({ person: null, couple })}
          lessonTrainers={lessonTrainers}
          disabled={
            remainingCapacity < (instance.capacityUnit === 'PEOPLE' ? 2 : 1)
          }
          onRegistered={() => refetch({ requestPolicy: 'network-only' })}
        />
      ))}
    </div>
  );
}
