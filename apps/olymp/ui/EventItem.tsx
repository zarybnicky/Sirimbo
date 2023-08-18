import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument } from '@app/graphql/Event';
import { fullDateFormatter } from '@app/ui/format-date';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { formatDefaultEventName, formatRegistrant } from '@app/ui/format-name';
import { Heading } from './Heading';
import { RegistrationForm } from './RegistrationForm';
import { NewRegistrationForm } from './NewRegistrationForm';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { buttonCls } from './style/button';

export const EventItem = ({ id }: { id: string }) => {
  const { user, perms } = useAuth();
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });
  const event = data?.event;
  const registrations = event?.eventRegistrationsList || [];
  const myRegistrations = registrations.filter(
    (x) => perms.isCurrentCouple(x.coupleId) || perms.isCurrentPerson(x.personId),
  );

  if (!event) return null;

  const total = event.eventRegistrationsList?.length ?? 0;
  return (
    <>
      <Heading>{event.name || formatDefaultEventName(event)}</Heading>

      <div>
        Termíny:{' '}
        {event.eventInstancesList.map((item) =>
          fullDateFormatter.formatRange(new Date(item.since), new Date(item.until)),
        )}
      </div>
      {parseInt(event.capacity) > 0 && (
        <div>
          Kapacita: Zbývá {event.remainingPersonSpots} míst z {event.capacity}
        </div>
      )}

      {event.eventTrainersList.length > 0 && (
        <div>
          Trenéři:{' '}
          {event.eventTrainersList.map((trainer) => (
            <span key={trainer.id}>
              {trainer.person!.firstName} {trainer.person!.lastName}{' '}
              {trainer.lessonsOffered > 0 &&
                `(zbývá ${trainer.lessonsRemaining} z ${trainer.lessonsOffered} lekcí)`}
            </span>
          ))}
        </div>
      )}
      <div>
        Místo konání:{' '}
        {event.locationText}
      </div>
      <RichTextView value={event.summary} />

      {perms.isLoggedIn && (
        <>
          <h3>Moje přihlášky</h3>
          {myRegistrations.map((reg) => (
            <RegistrationForm key={reg.id} event={event} registration={reg} />
          ))}
        </>
      )}
      <NewRegistrationForm event={event} />

      {total > 0 && (
        <Dialog>
          <DialogTrigger asChild>
            <button className={buttonCls()}>Účastníci ({total})</button>
          </DialogTrigger>
          <DialogContent>
            {perms.isTrainerOrAdmin && <EventParticipantExport id={event.id} />}
            {event.eventRegistrationsList?.map((x) => (
              <div key={x.id}>{formatRegistrant(x)}</div>
            ))}
          </DialogContent>
        </Dialog>
      )}

      <RichTextView value={event.description} />
      {!!user && <RichTextView value={event.descriptionMember} />}
    </>
  );
};
