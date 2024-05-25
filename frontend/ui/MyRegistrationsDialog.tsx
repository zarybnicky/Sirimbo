import { EventFragment } from '@/graphql/Event';
import { MyRegistrationCard } from '@/ui/MyRegistrationCard';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import Link from 'next/link';
import * as React from 'react';
import { NewRegistrationForm } from './NewRegistrationForm';

export function MyRegistrationsDialog({ event }: { event: EventFragment }) {
  const myRegistrations = event?.myRegistrationsList || [];

  if (
    event.isLocked ||
    event.eventInstancesList.every(i => new Date(i.since) < new Date()) ||
    (event.capacity > 0 && (event.remainingPersonSpots ?? 0) <= 0 && myRegistrations.length == 0)
  ) {
    return null;
  }

  return (
    <div className="flex flex-wrap gap-3">
      <Dialog modal={false}>
        {myRegistrations.length > 0 ? <DialogTrigger text="Moje přihlášky" /> : <DialogTrigger.Add text="Přihlásit" />}

        <DialogContent>
          <DialogTitle>Moje přihlášky</DialogTitle>

          {myRegistrations.map((reg) => (
            <MyRegistrationCard key={reg.id} event={event} registration={reg} />
          ))}

          {myRegistrations.length > 0 && <div className="text-lg font-bold">Další přihlášky</div>}

          {(event.capacity <= 0 || event.remainingPersonSpots) && (
            <NewRegistrationForm event={event} />
          )}
        </DialogContent>
      </Dialog>

      <Link href={`/akce/${event.id}`} className={buttonCls({ variant: 'outline' })}>
        Více info...
      </Link>
    </div>
  );
};
