import type { EventFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { RichTextView } from '@/ui/RichTextView';
import { formatEventType, formatOpenDateRange } from '@/ui/format';
import * as React from 'react';

export function BasicEventInfo({ event }: { event: EventFragment; }) {
  return (
    <dl className="not-prose gap-2 mb-6">
      <dd>{formatEventType(event)}</dd>
      <dt>Termíny</dt>
      <dd>
        {event.eventInstancesList.map(formatOpenDateRange).join(', ')}
      </dd>

      {event.capacity > 0 && (
        <>
          <dt>Kapacita</dt>
          <dd>Zbývá {event.remainingPersonSpots} míst z {event.capacity}</dd>
        </>
      )}

      {!!event.location?.name && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.location.name}</dd>
        </>
      )}
      {!!event.locationText && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.locationText}</dd>
        </>
      )}

      {event.eventTrainersList.length > 0 && (
        <>
          <dt>Trenéři</dt>
          {event.eventTrainersList.map((trainer) => (
            <dd key={trainer.id}>
              {trainer.name}
              {trainer.lessonsOffered > 0 &&
                ` (zbývá ${trainer.lessonsRemaining} z ${trainer.lessonsOffered} lekcí)`}
            </dd>
          ))}
        </>
      )}

      <dt>
        <MyRegistrationsDialog event={event} />
      </dt>

      {event.summary?.trim() && (
        <>
          <dt>Shrnutí</dt>
          <dd>
            <RichTextView value={event.summary} />
          </dd>
        </>
      )}
    </dl>
  );
}
