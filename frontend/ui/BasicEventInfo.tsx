import type { EventWithTrainerFragment } from '@/graphql/Event';
import { MyRegistrationsDialog } from '@/ui/MyRegistrationsDialog';
import { RichTextView } from '@/ui/RichTextView';
import { dateTimeFormatter, formatEventType } from '@/ui/format';
import Link from 'next/link';

export function BasicEventInfo({ instance }: { instance: EventWithTrainerFragment }) {
  const { seriesInfo } = instance;
  return (
    <dl className="not-prose gap-2 mb-6">
      {seriesInfo?.id && seriesInfo.length !== null && seriesInfo.length > 1 && (
        <dd>
          <Link
            href={`/terminy/${seriesInfo.id}`}
            className="text-xs underline decoration-neutral-7 underline-offset-2 hover:text-accent-11"
          >
            {seriesInfo.position}. z {seriesInfo.length} v sérii {seriesInfo.name?.trim()}
          </Link>
        </dd>
      )}

      <dd>{formatEventType(instance.type)}</dd>

      <dt>Termín</dt>
      <dd>
        {dateTimeFormatter.formatRange(
          new Date(instance.since),
          new Date(instance.until),
        )}
      </dd>

      {!!(instance.location?.name || instance.locationText) && (
        <>
          <dt>Místo konání</dt>
          <dd>{instance.location?.name || instance.locationText}</dd>
        </>
      )}

      {instance.trainersList.length > 0 && (
        <>
          <dt>Trenéři</dt>
          {instance.trainersList.map((trainer) => {
            return <dd key={trainer.id}>{trainer.person?.name}</dd>;
          })}
        </>
      )}

      {!!instance.capacity && (
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
