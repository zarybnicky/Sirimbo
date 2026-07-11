import {
  EventDocument,
  EventInstanceRegistrationsDocument,
  EventFragment,
  type EventInstanceWithTrainerFragment,
  type EventRegistrationFragment,
  SetEventInstanceRegistrationDocument,
} from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { NewRegistrationForm } from '@/ui/forms/NewRegistrationForm';
import { useAuth } from '@/ui/use-auth';
import { NewExternalRegistrationForm } from './forms/NewExternalRegistrationForm';
import { useMutation, useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { formatRegistrant } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import { useConfirm } from '@/ui/Confirm';
import { toast } from 'react-toastify';
import { LessonDemandControls } from '@/ui/forms/MyRegistrationForm';

export function MyRegistrationsDialog({
  instance,
  allowInstanceRegistration = false,
}: {
  instance: EventInstanceWithTrainerFragment;
  allowInstanceRegistration?: boolean;
}) {
  const auth = useAuth();
  const isInstanceRegistration = !instance.eventId;
  const isExternalRegistration = !auth.isLoggedIn || auth.personIds.length === 0;
  const myRegistrations = instance.myRegistrationsList ?? [];

  if (
    instance.isCancelled ||
    instance.isLocked ||
    new Date(instance.until) < new Date() ||
    (isInstanceRegistration &&
      (!allowInstanceRegistration ||
        (!instance.isPublic && !instance.isVisible))) ||
    ((!isInstanceRegistration || isExternalRegistration) &&
      (instance.capacity ?? 0) > 0 &&
      (instance.remainingPersonSpots ?? 0) <= 0 &&
      myRegistrations.length === 0)
  ) {
    return null;
  }

  return (
    <Dialog modal={false}>
      {isInstanceRegistration ? (
        <DialogTrigger size="sm" text="Přihlášky" />
      ) : myRegistrations.length > 0 ? (
        <DialogTrigger size="sm" text="Moje přihlášky" />
      ) : (
        <DialogTrigger.Add size="sm" text="Přihlásit" />
      )}

      <DialogContent>
        {isInstanceRegistration ? (
          <InstanceRegistrationsDialogContent instance={instance} />
        ) : (
          <MyRegistrationsDialogContent
            instance={instance}
            myRegistrations={instance.myRegistrationsList ?? []}
          />
        )}
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
  const confirm = useConfirm();
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
  const changeRegistration = useAsyncCallback(
    async (registration: { personId: string | null; coupleId: string | null }, registered: boolean) => {
      if (!registered) {
        await confirm({ description: 'Opravdu chcete zrušit přihlášku?' });
      }
      const result = await setRegistration({
        input: {
          pInstanceId: instance.id,
          pPersonId: registration.personId,
          pCoupleId: registration.coupleId,
          pIsRegistered: registered,
        },
      });
      if (result.error) throw result.error;
      reloadRegistrations({ requestPolicy: 'network-only' });
      toast.success(registered ? 'Přihlášení proběhlo úspěšně.' : 'Přihláška zrušena.');
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
      <FormError error={changeRegistration.error} />

      {myRegistrations.map((registration) => (
        <div key={registration.id} className="space-y-2">
          <div className="flex items-center justify-between gap-2">
            <b>{formatRegistrant(registration)}</b>
            <SubmitButton
              type="button"
              variant="outline"
              loading={changeRegistration.loading}
              onClick={() => changeRegistration.execute(registration, false)}
            >
              Zrušit přihlášku
            </SubmitButton>
          </div>
          <LessonDemandControls
            registrationId={registration.id}
            demands={registration.eventLessonDemandsByRegistrationIdList}
            trainers={lessonTrainers}
          />
        </div>
      ))}

      {(availablePeople.length > 0 || availableCouples.length > 0) &&
        myRegistrations.length > 0 && <div className="text-lg font-bold">Další přihlášky</div>}

      {availablePeople.map((person) => (
        <div key={person.id} className="flex items-center justify-between gap-2">
          <span>{person.name}</span>
          <SubmitButton
            type="button"
            loading={changeRegistration.loading}
            disabled={remainingCapacity < 1}
            onClick={() =>
              changeRegistration.execute({ personId: person.id, coupleId: null }, true)
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
            loading={changeRegistration.loading}
            disabled={
              remainingCapacity < (instance.capacityUnit === 'PEOPLE' ? 2 : 1)
            }
            onClick={() =>
              changeRegistration.execute({ personId: null, coupleId: couple.id }, true)
            }
          >
            Přihlásit
          </SubmitButton>
        </div>
      ))}
    </div>
  );
}

function MyRegistrationsDialogContent({
  instance,
  myRegistrations,
}: {
  instance: EventInstanceWithTrainerFragment;
  myRegistrations: EventRegistrationFragment[];
}) {
  const auth = useAuth();
  const [{ data, fetching }] = useQuery({
    query: EventDocument,
    variables: { id: instance.eventId! },
    pause: !instance.eventId,
  });
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
  });
  const event: EventFragment | undefined = data?.event ?? undefined;

  if (!event || !registrationsQuery.data) {
    return (
      <>
        <FormError error={registrationsQuery.error} />
        {!registrationsQuery.error && (
          <div className="text-sm text-neutral-10">
            {fetching || instance.eventId ? 'Načítám…' : 'Přihlášky nejsou k dispozici.'}
          </div>
        )}
      </>
    );
  }

  const instanceRegistrations =
    registrationsQuery.data.eventInstance?.registrations.nodes ?? [];
  const lessonTrainers =
    registrationsQuery.data.eventInstance?.lessonTrainers ?? [];

  return (
    <>
      {myRegistrations.map((registration) => {
        const instanceRegistration = instanceRegistrations.find(
          (candidate) =>
            candidate.personId === registration.personId &&
            candidate.coupleId === registration.coupleId,
        );
        return (
          <MyRegistrationCard
            key={registration.id}
            event={event}
            registration={registration}
            instanceRegistrationId={instanceRegistration?.id ?? null}
            lessonDemands={
              instanceRegistration?.eventLessonDemandsByRegistrationIdList ?? []
            }
            lessonTrainers={lessonTrainers}
          />
        );
      })}

      {((instance.capacity ?? 0) <= 0 || (instance.remainingPersonSpots ?? 0) > 0) && (
        <>
          {myRegistrations.length > 0 && (
            <div className="text-lg font-bold">Další přihlášky</div>
          )}
          {auth.isLoggedIn && auth.personIds.length > 0 ? (
            <NewRegistrationForm event={event} lessonTrainers={lessonTrainers} />
          ) : (
            <NewExternalRegistrationForm instanceId={instance.id} />
          )}
        </>
      )}
    </>
  );
}
