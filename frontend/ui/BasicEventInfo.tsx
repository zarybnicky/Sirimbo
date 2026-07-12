import type { EventWithTrainerFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { RichTextView } from '@/ui/RichTextView';
import { SeriesInfoLink } from '@/ui/SeriesInfoLink';
import { dateTimeFormatter, formatEventType,  } from '@/ui/format';

export function BasicEventInfo({ instance }: { instance: EventWithTrainerFragment }) {
  return (
    <dl className="not-prose gap-2 mb-6">
      {(instance.seriesInfo?.length ?? 0) > 1 && (
        <dd>
          <SeriesInfoLink info={instance.seriesInfo} />
        </dd>
      )}

      <dd>{formatEventType(instance.type)}</dd>

      <dt>Termín</dt>
      <dd>{dateTimeFormatter.formatRange(new Date(instance.since), new Date(instance.until))}</dd>

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

      {(instance.capacity ?? 0) > 0 && (
        <>
          <dt>Kapacita</dt>
          <dd>
            Zbývá {instance.remainingPersonSpots} míst z {instance.capacity}
          </dd>
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
