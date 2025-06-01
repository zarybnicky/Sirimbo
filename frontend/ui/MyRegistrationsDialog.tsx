import type { EventFragment } from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { NewRegistrationForm } from '@/ui/forms/NewRegistrationForm';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { NewExternalRegistrationForm } from './forms/NewExternalRegistrationForm';

export function MyRegistrationsDialog({ event }: { event: EventFragment }) {
  const auth = useAuth();

  const myRegistrations = event?.myRegistrationsList || [];

  if (
    event.isLocked ||
    event.eventInstancesList.every(i => new Date(i.since) < new Date()) ||
    (event.capacity > 0 && (event.remainingPersonSpots ?? 0) <= 0 && myRegistrations.length === 0)
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
        <DialogTitle>
          {myRegistrations.length > 0 ? "Moje přihlášky" : "Přihlášení na akci"}
        </DialogTitle>

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
      </DialogContent>
    </Dialog>
  );
};
