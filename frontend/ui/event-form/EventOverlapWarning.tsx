import { EventFormOverlapsDocument } from '@/graphql/EventConflicts';
import { dateTimeFormatter, formatEventName } from '@/ui/format';
import { AlertTriangle } from 'lucide-react';
import { useQuery } from 'urql';

type DraftInstance = {
  since: string;
  until: string;
  itemId?: string | null;
  isCancelled?: boolean;
  people: string[];
};

export function useEventOverlaps(instances: DraftInstance[], parentId?: string | null) {
  const active = instances.filter((x) => !x.isCancelled && Date.parse(x.since) < Date.parse(x.until));
  const people = [...new Set(active.flatMap((x) => x.people))].toSorted();
  const start = active.length > 0 ? new Date(Math.min(...active.map((x) => Date.parse(x.since)))).toISOString() : '';
  const end = active.length > 0 ? new Date(Math.max(...active.map((x) => Date.parse(x.until)))).toISOString() : '';
  const paused = active.length === 0 || people.length === 0;
  const [{ data, fetching, error }] = useQuery({
    query: EventFormOverlapsDocument,
    variables: { people, range: { start, end } },
    pause: paused,
  });
  const byPerson: Record<string, string[]> = {};
  const byInstance: string[][] = instances.map(() => []);
  if (!paused && !fetching && !error) {
    for (const event of data?.eventsList ?? []) {
      if (event.id === parentId || instances.some((x) => x.itemId === event.id)) continue;
      const participants = new Map([...event.eventTrainersList, ...event.registrationsList]
        .flatMap((entry) => entry.personId && entry.person ? [[entry.personId, entry.person.name] as const] : []));
      const description = `${formatEventName(event)} (${dateTimeFormatter.formatRange(new Date(event.since), new Date(event.until))})`;
      for (const [index, instance] of instances.entries()) {
        if (!active.includes(instance) || (instance.itemId && event.parentId === instance.itemId)
          || Date.parse(event.since) >= Date.parse(instance.until)
          || Date.parse(event.until) <= Date.parse(instance.since)) continue;
        for (const id of new Set(instance.people)) {
          const name = participants.get(id);
          if (!name) continue;
          const line = `${name}: ${description}`;
          (byPerson[id] ??= []).push(line);
          byInstance[index]!.push(line);
        }
      }
    }
  }
  return { byPerson, byInstance };
}

export function EventOverlapWarning({ lines = [] }: { lines?: string[] }) {
  if (lines.length === 0) return null;
  const title = `Časový konflikt:\n${[...new Set(lines)].join('\n')}`;
  return (
    <span
      title={title}
      aria-label={title}
      role="img"
      tabIndex={0}
      className="inline-flex text-accent-11"
    >
      <AlertTriangle className="size-4" aria-hidden="true" />
    </span>
  );
}
