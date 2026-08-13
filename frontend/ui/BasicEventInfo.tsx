import type { ISharedEventResult } from '@/app/(member)/termin/[id]/termin.queries';
import type { EventPageFragment } from '@/graphql/Event';
import { RichTextView } from '@/ui/RichTextView';
import { dateTimeFormatter } from '@/ui/format';
import Link from 'next/link';

export function BasicEventInfo({
  instance,
}: {
  instance: EventPageFragment | ISharedEventResult;
}) {
  const shared = 'trainerNames' in instance;
  const seriesInfo = shared ? null : instance.seriesInfo;
  const location =
    (shared ? instance.locationName : instance.location?.name) || instance.locationText;
  const trainers = shared
    ? instance.trainerNames
    : instance.trainersList.map((t) => t.person?.name).filter(Boolean);

  return (
    <div className="flex flex-col gap-3">
      {!shared && seriesInfo?.id && seriesInfo.length !== null && seriesInfo.length > 1 && (
        <div>
          <Link
            href={`/terminy/${seriesInfo.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {seriesInfo.position}. z {seriesInfo.length} v sérii{' '}
            {seriesInfo.name?.trim()}
          </Link>
        </div>
      )}

      {!shared && instance.parent && (
        <div>
          <Link
            href={`/termin/${instance.parent.id}`}
            className="text-xs hover:text-accent-11 underline"
          >
            {instance.parent.name}
          </Link>
        </div>
      )}

      <dl className="not-prose tabular">
        <dt>Termín</dt>
        <dd>
          {dateTimeFormatter.formatRange(
            new Date(instance.since),
            new Date(instance.until),
          )}
        </dd>

        {location && (
          <>
            <dt>Místo konání</dt>
            <dd>{location}</dd>
          </>
        )}

        {trainers.length > 0 && (
          <>
            <dt>Trenéři</dt>
            {trainers.map((trainer, index) => (
              <dd key={`${trainer}:${index}`}>{trainer}</dd>
            ))}
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
      </dl>

      {instance.summary?.trim() && <RichTextView value={instance.summary} />}
      {!shared && instance.description && <RichTextView value={instance.description} />}
    </div>
  );
}
