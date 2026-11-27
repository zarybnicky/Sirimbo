import { EventFormOverlapsDocument } from '@/graphql/EventConflicts';
import { dateTimeFormatter, formatEventName } from '@/ui/format';
import { AlertTriangle } from 'lucide-react';
import { useQuery } from 'urql';

type DraftInstance = {
  since: string;
  until: string;
  itemId?: string | null;
  isCancelled?: boolean;
  personIds: string[];
};

export function useEventOverlaps(instances: DraftInstance[], parentId?: string | null) {
  const active = instances.filter(
    (x) => !x.isCancelled && Date.parse(x.since) < Date.parse(x.until),
  );
  const personIds = [...new Set(active.flatMap((x) => x.personIds))].toSorted();
  const start =
    active.length > 0
      ? new Date(Math.min(...active.map((x) => Date.parse(x.since)))).toISOString()
      : '';
  const end =
    active.length > 0
      ? new Date(Math.max(...active.map((x) => Date.parse(x.until)))).toISOString()
      : '';
  const paused = active.length === 0 || personIds.length === 0;
  const [{ data, fetching, error }] = useQuery({
    query: EventFormOverlapsDocument,
    variables: { personIds, range: { start, end } },
    pause: paused,
  });
  if (paused || fetching || error) return { byPerson: {}, byInstance: [] };

  const byPerson: Record<string, string[]> = {};
  const byInstance: string[][] = instances.map(() => []);

  for (const event of data?.eventsList ?? []) {
    if (event.id === parentId || instances.some((x) => x.itemId === event.id)) continue;
    const participants = new Map(
      [...event.eventTrainersList, ...event.registrationsList].flatMap((x) =>
        x.personId ? [[x.personId, x.person?.name] as const] : [],
      ),
    );

    for (const [index, instance] of instances.entries()) {
      if (
        !active.includes(instance) ||
        (instance.itemId && event.parentId === instance.itemId) ||
        Date.parse(event.since) >= Date.parse(instance.until) ||
        Date.parse(event.until) <= Date.parse(instance.since)
      )
        continue;
      for (const id of new Set(instance.personIds)) {
        const name = participants.get(id);
        if (!name) continue;
        const line = `${name}: ${formatEventName(event)} (${dateTimeFormatter.formatRange(new Date(event.since), new Date(event.until))})`;
        (byPerson[id] ??= []).push(line);
        byInstance[index]!.push(line);
      }
    }
  }
  return { byPerson, byInstance };
}

export function EventOverlapWarning({ lines }: { lines?: string[] }) {
  if (!lines?.length) return null;
  return (
    <span
      title={`Časový konflikt:\n${[...new Set(lines)].join('\n')}`}
      className="inline-flex text-accent-11"
    >
      <AlertTriangle className="size-4" />
    </span>
  );
}
