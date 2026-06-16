import type { EventFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { RichTextView } from '@/ui/RichTextView';
import { formatEventType, formatOpenDateRange } from '@/ui/format';

export function BasicEventInfo({ event }: { event: EventFragment }) {
  return (
    <dl className="not-prose gap-2 mb-6">
      <dd>{formatEventType(event.type)}</dd>
      <dt>Termíny</dt>
      <dd>{event.eventInstancesList.map(formatOpenDateRange).join(', ')}</dd>

      {event.capacity > 0 && (
        <>
          <dt>Kapacita</dt>
          <dd>
            Zbývá {event.remainingPersonSpots} míst z {event.capacity}
          </dd>
        </>
      )}

      {!!(event.location?.name || event.locationText) && (
        <>
          <dt>Místo konání</dt>
          <dd>{event.location?.name || event.locationText}</dd>
        </>
      )}

      {event.eventTrainersList.length > 0 && (
        <>
          <dt>Trenéři</dt>
          {event.eventTrainersList.map((trainer) => {
            const lessonsOffered = trainer.lessonsOffered as number | null;
            return (
              <dd key={trainer.id}>
                {trainer.name}
                {lessonsOffered === null ? ' (bez omezení)' :
                  lessonsOffered === 0 ? '' :
                    ` (zbývá ${trainer.lessonsRemaining ?? 0} z ${lessonsOffered} lekcí)`}
              </dd>
            );
          })}
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
