import type { EventPageFragment } from '@/graphql/Event';
import { isTruthy } from '@/lib/truthyFilter';
import { RichTextView } from '@/ui/RichTextView';
import { dateTimeFormatter } from '@/ui/format';
import Link from 'next/link';

export function BasicEventInfo({ event }: { event: EventPageFragment }) {
  const trainers = event.trainersList.map((t) => t.person).filter(isTruthy);
  const { seriesInfo } = event;

  return (
    <div className="flex flex-col gap-3">
      {seriesInfo?.id && (seriesInfo.length ?? 0) > 1 && (
        <div>
          <Link
            href={`/terminy/${seriesInfo.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {seriesInfo.position}. z {seriesInfo.length} v sérii {seriesInfo.name?.trim()}
          </Link>
        </div>
      )}

      {event.parent && (
        <div>
          <Link
            href={`/termin/${event.parent.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {event.parent.name}
          </Link>
        </div>
      )}

      <dl className="not-prose tabular">
        <dt>Termín</dt>
        <dd>
          {dateTimeFormatter.formatRange(new Date(event.since), new Date(event.until))}
        </dd>

        {(event.location?.name || event.locationText) && (
          <>
            <dt>Místo konání</dt>
            <dd>
              {event.location ? (
                <Link className="underline" href={`/lokality/${event.location.id}`}>
                  {event.location.name}
                </Link>
              ) : (
                event.locationText
              )}
            </dd>
          </>
        )}

        {trainers.length > 0 && (
          <>
            <dt>Trenéři</dt>
            {trainers.map((x) => (
              <dd key={x.id}>{x.name}</dd>
            ))}
          </>
        )}

        {!!event.capacity && (
          <>
            <dt>Kapacita</dt>
            <dd>
              Zbývá {event.registrationInfo?.remainingCapacity} míst z {event.capacity}
            </dd>
          </>
        )}
      </dl>

      <RichTextView value={event.summary} />
      <RichTextView value={event.description} />
    </div>
  );
}
