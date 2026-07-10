import { EventDocument, EventFragment, type EventInstanceWithTrainerFragment, type EventRegistrationFragment } from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { NewRegistrationForm } from '@/ui/forms/NewRegistrationForm';
import { useAuth } from '@/ui/use-auth';
import { NewExternalRegistrationForm } from './forms/NewExternalRegistrationForm';
import { useQuery } from 'urql';

export function MyRegistrationsDialog({ instance }: { instance: EventInstanceWithTrainerFragment }) {
  const myRegistrations = instance?.myRegistrationsList || [];

  if (
    !instance.eventId ||
    instance.isLocked ||
    new Date(instance.until) < new Date() ||
      ((instance.capacity ?? 0) > 0 && (instance.remainingPersonSpots ?? 0) <= 0 &&
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
        <MyRegistrationsDialogContent
          eventId={instance.eventId}
          myRegistrations={myRegistrations}
        />
      </DialogContent>
    </Dialog>
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
