import { add } from 'date-arithmetic';
import type { CalendarEvent, CalendarInstanceEvent, SlotInfo } from '@/calendar/types';

export type CreateEventDefaults = {
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

export function defaultsFromSlot(
  slot: SlotInfo,
  events: readonly CalendarEvent[],
  persons: readonly AuthPerson[],
  onlyMine: null | boolean,
): CreateEventDefaults {
  const [resourceType, resourceId] = parseResourceKey(slot.resource?.resourceId);
  const trainerPersonIds: string[] = [];

  if (resourceType === 'person' && resourceId) {
    trainerPersonIds.push(resourceId);
  } else if (onlyMine && !slot.resource) {
    const trainer = persons.find((person) => person.isTrainer);
    if (trainer) trainerPersonIds.push(trainer.id);
  }

  const defaults: CreateEventDefaults = {
    since: slot.start,
    until: slot.action === 'click' ? add(slot.start, 45, 'minutes') : slot.end,
    trainerPersonIds,
    locationId: null,
    locationText: '',
  };

  if (resourceType === 'location' && resourceId) {
    defaults.locationId = resourceId;
  } else if (resourceType === 'locationText' && resourceId) {
    defaults.locationText = resourceId;
  } else if (trainerPersonIds[0]) {
    const closest = findClosest(events, trainerPersonIds[0], defaults.since);
    if (closest?.instance.locationText) {
      defaults.locationText = closest.instance.locationText;
    }
    if (closest?.instance.location?.id) {
      defaults.locationId = closest.instance.location.id;
      defaults.locationText = '';
    }
  }

  return defaults;
}

function findClosest(events: readonly CalendarEvent[], personId: string, since: Date) {
  let closestPrev: CalendarInstanceEvent | undefined;
  const dateKey = since.toISOString().slice(0, 10);

  for (const event of events) {
    if (
      event.kind === 'event' &&
      event.start.toISOString().slice(0, 10) === dateKey &&
      event.instance.trainersList?.some((x) => x.personId === personId) &&
      event.end <= since &&
      (!closestPrev || closestPrev.start < event.start)
    ) {
      closestPrev = event;
    }
  }

  return closestPrev;
}
