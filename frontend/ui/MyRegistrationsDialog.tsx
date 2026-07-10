import {
  EventDocument,
  EventInstanceRegistrationsDocument,
  type EventInstanceRegistrationsQuery,
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

type InstanceRegistration = NonNullable<
  EventInstanceRegistrationsQuery['eventInstance']
>['registrations']['nodes'][number];

export function MyRegistrationsDialog({
  instance,
  allowInstanceRegistration = false,
}: {
  instance: EventInstanceWithTrainerFragment;
  allowInstanceRegistration?: boolean;
}) {
  const auth = useAuth();
  const isInstanceRegistration = !instance.eventId;
  const [registrationsQuery, reloadRegistrations] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id: instance.id },
    pause: !isInstanceRegistration || !allowInstanceRegistration || !auth.isLoggedIn,
  });
  const instanceRegistrations =
    registrationsQuery.data?.eventInstance?.registrations.nodes ?? [];
  const myInstanceRegistrations = instanceRegistrations.filter(
    (registration) =>
      (!!registration.personId && auth.isMyPerson(registration.personId)) ||
      (!!registration.coupleId && auth.isMyCouple(registration.coupleId)),
  );
  const myRegistrations = isInstanceRegistration
    ? myInstanceRegistrations
    : (instance.myRegistrationsList ?? []);

  if (
    instance.isCancelled ||
    instance.isLocked ||
    new Date(instance.until) < new Date() ||
    (isInstanceRegistration &&
      (!allowInstanceRegistration ||
        !auth.isLoggedIn ||
        auth.personIds.length === 0 ||
        (!instance.isPublic && !instance.isVisible))) ||
    ((instance.capacity ?? 0) > 0 &&
      (instance.remainingPersonSpots ?? 0) <= 0 &&
      myRegistrations.length === 0)
  ) {
    return null;
  }

  return (
    <Dialog modal={false}>
      {myRegistrations.length > 0 ? (
        <DialogTrigger size="sm" text="Moje přihlášky" />
      ) : (
        <DialogTrigger.Add size="sm" text="Přihlásit" />
      )}

      <DialogContent>
        {isInstanceRegistration ? (
          <InstanceRegistrationsDialogContent
            instance={instance}
            registrations={instanceRegistrations}
            myRegistrations={myInstanceRegistrations}
            reload={() => reloadRegistrations({ requestPolicy: 'network-only' })}
          />
        ) : (
          <MyRegistrationsDialogContent
            eventId={instance.eventId}
            myRegistrations={instance.myRegistrationsList ?? []}
          />
        )}
      </DialogContent>
    </Dialog>
  );
}

function InstanceRegistrationsDialogContent({
  instance,
  registrations,
  myRegistrations,
  reload,
}: {
  instance: EventInstanceWithTrainerFragment;
  registrations: InstanceRegistration[];
  myRegistrations: InstanceRegistration[];
  reload: () => void;
}) {
  const auth = useAuth();
  const confirm = useConfirm();
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
      reload();
      toast.success(registered ? 'Přihlášení proběhlo úspěšně.' : 'Přihláška zrušena.');
    },
  );

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
        <div key={registration.id} className="flex items-center justify-between gap-2">
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
  eventId,
  myRegistrations,
}: {
  eventId: string | null;
  myRegistrations: EventRegistrationFragment[];
}) {
  const auth = useAuth();
  const [{ data, fetching }] = useQuery({
    query: EventDocument,
    variables: { id: eventId! },
    pause: !eventId,
  });
  const event: EventFragment | undefined = data?.event ?? undefined;

  if (!event) {
    return (
      <div className="text-sm text-neutral-10">
        {fetching || eventId ? 'Načítám…' : 'Přihlášky nejsou k dispozici.'}
      </div>
    );
  }

  return (
    <>
      {myRegistrations.map((reg) => (
        <MyRegistrationCard key={reg.id} event={event} registration={reg} />
      ))}

      {(event.capacity <= 0 || (event.remainingPersonSpots ?? 0) > 0) && (
        <>
          {myRegistrations.length > 0 && (
            <div className="text-lg font-bold">Další přihlášky</div>
          )}
          {auth.isLoggedIn && auth.personIds.length > 0 ? (
            <NewRegistrationForm event={event} />
          ) : (
            <NewExternalRegistrationForm event={event} />
          )}
        </>
      )}
    </>
  );
}
