import type { EventInstanceWithTrainerFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { RichTextView } from '@/ui/RichTextView';
import { formatEventType } from '@/ui/format';

export function BasicEventInfo({ instance }: { instance: EventInstanceWithTrainerFragment }) {
  return (
    <dl className="not-prose gap-2 mb-6">
      <dd>{formatEventType(instance.type)}</dd>

      {(instance.capacity ?? 0) > 0 && (
        <>
          <dt>Kapacita</dt>
          <dd>
            Zbývá {instance.remainingPersonSpots} míst z {instance.capacity}
          </dd>
        </>
      )}

      {!!(instance.location?.name || instance.locationText) && (
        <>
          <dt>Místo konání</dt>
          <dd>{instance.location?.name || instance.locationText}</dd>
        </>
      )}

      {instance.trainersList && instance.trainersList.length > 0 && (
        <>
          <dt>Trenéři</dt>
          {instance.trainersList.map((trainer) => {
            return (
              <dd key={trainer.id}>
                {trainer.person?.name}
              </dd>
            );
          })}
        </>
      )}

      <dt>
        <MyRegistrationsDialog instance={instance} />
      </dt>

      {instance.summary?.trim() && (
        <>
          <dt>Shrnutí</dt>
          <dd>
            <RichTextView value={instance.summary} />
          </dd>
        </>
      )}
    </dl>
  );
}
