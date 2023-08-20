import { RichTextView } from '@app/ui/RichTextView';
import { EventDocument } from '@app/graphql/Event';
import { fullDateFormatter } from '@app/ui/format-date';
import { useAuth } from '@app/ui/use-auth';
import * as React from 'react';
import { EventParticipantExport } from './EventParticipantExport';
import { useQuery } from 'urql';
import { formatDefaultEventName, formatEventType, formatRegistrant } from '@app/ui/format-name';
import { TitleBar } from './TitleBar';
import { RegistrationForm } from './RegistrationForm';
import { NewRegistrationForm } from './NewRegistrationForm';
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from './accordion';

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
      <TitleBar title={event.name || formatDefaultEventName(event)}>
        <NewRegistrationForm event={event} />
      </TitleBar>

      <dl className="gap-2 mb-6">
        <dt>{formatEventType(event)}</dt>
        <dt>Termíny</dt>
        <dd>
          {event.eventInstancesList.map((item) =>
            fullDateFormatter.formatRange(new Date(item.since), new Date(item.until)),
          ).join(', ')}
        </dd>

        {parseInt(event.capacity) > 0 && (
          <>
            <dt>Kapacita</dt>
            <dd>Zbývá {event.remainingPersonSpots} míst z {event.capacity}</dd>
          </>
        )}

        {event.eventTrainersList.length > 0 && (
          <>
            <dt>Trenéři</dt>
            {event.eventTrainersList.map((trainer) => (
              <dd key={trainer.id}>
                {trainer.person!.firstName} {trainer.person!.lastName}{' '}
                {trainer.lessonsOffered > 0 &&
                 `(zbývá ${trainer.lessonsRemaining} z ${trainer.lessonsOffered} lekcí)`}
              </dd>
            ))}
          </>
        )}

        <dt>Místo konání</dt>
        <dd>{event.locationText}</dd>

        {event.summary?.trim() && (
          <>
            <dt>Shrnutí</dt>
            <dd>
              <RichTextView value={event.summary} />
            </dd>
          </>
        )}

      </dl>

      <Accordion type="multiple" defaultValue={['info', 'myRegistrations']}>
        {myRegistrations.length > 0 && (
          <AccordionItem value="myRegistrations">
            <AccordionTrigger className="font-bold">Moje přihlášky</AccordionTrigger>
            <AccordionContent>
              {myRegistrations.map((reg) => (
                <RegistrationForm key={reg.id} event={event} registration={reg} />
              ))}
            </AccordionContent>
          </AccordionItem>
        )}

        <AccordionItem value="info">
          <AccordionTrigger className="font-bold">
            Informace
          </AccordionTrigger>
          <AccordionContent>
            <RichTextView value={event.description} />
            {!!user && <RichTextView value={event.descriptionMember} />}
          </AccordionContent>
        </AccordionItem>

        {total > 0 && (
          <AccordionItem value="participants">
            <AccordionTrigger className="font-bold">
              Účastníci ({total})
            </AccordionTrigger>
            <AccordionContent>
              {perms.isTrainerOrAdmin && <EventParticipantExport id={event.id} />}
              {event.eventRegistrationsList?.map((x) => (
                <div key={x.id}>{formatRegistrant(x)}</div>
              ))}
            </AccordionContent>
          </AccordionItem>
        )}
      </Accordion>
    </>
  );
};
