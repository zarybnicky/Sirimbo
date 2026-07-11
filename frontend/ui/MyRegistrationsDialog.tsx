import {
  EventInstanceRegistrationsDocument,
  type EventInstanceWithTrainerFragment,
  SetEventInstanceRegistrationDocument,
} from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { useAuth } from '@/ui/use-auth';
import { NewExternalRegistrationForm } from './forms/NewExternalRegistrationForm';
import { useMutation, useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { formatRegistrant } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';

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
  const [registrationsQuery, reloadRegistrations] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
    pause: !auth.isLoggedIn || auth.personIds.length === 0,
  });
  const registrations =
    registrationsQuery.data?.eventInstance?.registrations.nodes ?? [];
  const lessonTrainers = registrationsQuery.data?.eventInstance?.lessonTrainers ?? [];
  const myRegistrations = registrations.filter(
    (registration) =>
      (!!registration.personId && auth.isMyPerson(registration.personId)) ||
      (!!registration.coupleId && auth.isMyCouple(registration.coupleId)),
  );
  const setRegistration = useMutation(SetEventInstanceRegistrationDocument)[1];
  const register = useAsyncCallback(
    async (registration: { personId: string | null; coupleId: string | null }) => {
      const result = await setRegistration({
        input: {
          pInstanceId: instance.id,
          pPersonId: registration.personId,
          pCoupleId: registration.coupleId,
          pIsRegistered: true,
        },
      });
      if (result.error) throw result.error;
      reloadRegistrations({ requestPolicy: 'network-only' });
      toast.success('Přihlášení proběhlo úspěšně.');
    },
  );

  if (!auth.isLoggedIn || auth.personIds.length === 0) {
    return <NewExternalRegistrationForm instanceId={instance.id} />;
  }

  if (!registrationsQuery.data) {
    return registrationsQuery.error ? (
      <FormError error={registrationsQuery.error} />
    ) : (
      <div className="text-sm text-neutral-10">Načítám…</div>
    );
  }

  const occupiedPersonIds = new Set(
    registrations.flatMap((registration) =>
      registration.personId
        ? [registration.personId]
        : [registration.couple?.man?.id, registration.couple?.woman?.id].filter(
            (id): id is string => !!id,
          ),
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
      <FormError error={register.error} />

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
        <div key={person.id} className="flex items-center justify-between gap-2">
          <span>{person.name}</span>
          <SubmitButton
            type="button"
            loading={register.loading}
            disabled={remainingCapacity < 1}
            onClick={() =>
              register.execute({ personId: person.id, coupleId: null })
            }
          >
            Přihlásit
          </SubmitButton>
        </div>
      ))}

      {availableCouples.map((couple) => (
        <div key={couple.id} className="flex items-center justify-between gap-2">
          <span>{formatRegistrant({ person: null, couple })}</span>
          <SubmitButton
            type="button"
            loading={register.loading}
            disabled={
              remainingCapacity < (instance.capacityUnit === 'PEOPLE' ? 2 : 1)
            }
            onClick={() =>
              register.execute({ personId: null, coupleId: couple.id })
            }
          >
            Přihlásit
          </SubmitButton>
        </div>
      ))}
    </div>
  );
}
