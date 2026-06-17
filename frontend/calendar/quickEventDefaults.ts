import { add } from 'date-arithmetic';
import { localDateKey } from '@/calendar/localizer';
import type {
  CalendarEvent,
  CalendarInstanceEvent,
  SlotInfo,
} from '@/calendar/types';

export type QuickEventCreateDefaults = {
  since: Date;
  until: Date;
  trainerPersonIds: string[];
  locationId: string | null;
  locationText: string;
};

type AuthPerson = {
  id: string;
  isTrainer: boolean | null;
};

export function parseResourceKey(key: string | undefined) {
  const pos = key?.indexOf(':') ?? -1;
  if (!key || pos === -1) return ['', ''] as const;
  return [key.slice(0, pos), key.slice(pos + 1)] as const;
}

export function splitIntoLessonRanges(since: Date, until: Date) {
  const ranges: { since: Date; until: Date }[] = [];
  let start = since;

  while (add(start, 45, 'minutes') <= until) {
    const end = add(start, 45, 'minutes');
    ranges.push({ since: start, until: end });
    start = end;
  }

  return ranges;
}

export function quickDefaultsFromSlot(
  slot: SlotInfo,
  events: readonly CalendarEvent[],
  persons: readonly AuthPerson[],
  onlyMine: null | boolean,
): QuickEventCreateDefaults {
  const until = slot.action === 'click' ? add(slot.start, 45, 'minutes') : slot.end;
  const [resourceType, resourceId] = parseResourceKey(slot.resource?.resourceId);
  const trainerPersonIds: string[] = [];

  if (resourceType === 'person' && resourceId) {
    trainerPersonIds.push(resourceId);
  } else if (onlyMine && !slot.resource) {
    const trainer = persons.find((person) => person.isTrainer);
    if (trainer) trainerPersonIds.push(trainer.id);
  }

  const defaults: QuickEventCreateDefaults = {
    since: slot.start,
    until,
    trainerPersonIds,
    locationId: null,
    locationText: '',
  };

  if (resourceType === 'location' && resourceId) {
    defaults.locationId = resourceId;
  } else if (resourceType === 'locationText' && resourceId) {
    defaults.locationText = resourceId;
  } else if (trainerPersonIds[0]) {
    const closestPrev = findClosestPreviousForTrainer(
      events,
      trainerPersonIds[0],
      defaults.since,
    );
    if (closestPrev?.instance.locationText) {
      defaults.locationText = closestPrev.instance.locationText;
    }
    if (closestPrev?.instance.location?.id) {
      defaults.locationId = closestPrev.instance.location.id;
      defaults.locationText = '';
    }
  }

  return defaults;
}

export function quickDefaultsAfterLessonGroup(
  items: readonly CalendarInstanceEvent[],
): QuickEventCreateDefaults {
  const last = items.at(-1);
  const since = last?.end ?? new Date();

  return {
    since,
    until: add(since, 45, 'minutes'),
    trainerPersonIds: last?.instance.trainersList?.map(({ personId }) => personId) ?? [],
    locationId: last?.instance.location?.id ?? null,
    locationText: last?.instance.locationText ?? '',
  };
}

function findClosestPreviousForTrainer(
  events: readonly CalendarEvent[],
  trainerPersonId: string,
  since: Date,
) {
  let closestPrev: CalendarInstanceEvent | undefined;
  const dateKey = localDateKey(since);

  for (const event of events) {
    if (event.kind !== 'event') continue;
    if (localDateKey(event.start) !== dateKey) continue;
    if (!event.instance.trainersList?.some((trainer) => trainer.personId === trainerPersonId)) {
      continue;
    }
    if (event.end > since) continue;
    if (!closestPrev || closestPrev.start < event.start) {
      closestPrev = event;
    }
  }

  return closestPrev;
}
