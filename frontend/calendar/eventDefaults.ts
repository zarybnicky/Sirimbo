import type { SlotInfo } from '@/calendar/types';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import type { EventFormType } from '@/ui/event-form/types';
import { add, eq } from 'date-arithmetic';

export type CreateEventDefaults = {
  date?: Date;
  after?: EventWithTrainerFragment;
  events?: readonly EventWithTrainerFragment[];
  type?: EventFormType['type'];
  since?: Date;
  until?: Date;
  trainerPersonIds?: readonly string[];
  locationId?: string | null;
  locationText?: string;
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

export function defaultsFromSlot(slot: SlotInfo): CreateEventDefaults {
  if (slot.action === 'day') return { date: slot.start };

  const [resourceType, resourceId] = parseResourceKey(slot.resource?.resourceId);
  return {
    type: resourceType === 'eventType' && resourceId === 'GROUP' ? 'GROUP' : undefined,
    since: slot.start,
    until: slot.action === 'select' ? slot.end : undefined,
    trainerPersonIds:
      resourceType === 'person' && resourceId
        ? [resourceId]
        : slot.resource && !resourceType
          ? []
          : undefined,
    locationId: resourceType === 'location' ? resourceId : undefined,
    locationText: resourceType === 'locationText' ? resourceId : undefined,
  };
}

export function createEventDefaults(
  input: CreateEventDefaults,
  fallback: { trainerPersonIds: readonly string[]; isLocked: boolean },
): EventFormType {
  const { after, events = [], type = 'LESSON' } = input;
  const since = new Date(input.since ?? after?.until ?? input.date ?? new Date());
  if (!input.since && !after) since.setHours(type === 'CAMP' ? 9 : 15, 0, 0, 0);
  const until = input.until ?? add(since, type === 'CAMP' ? 480 : 45, 'minutes');
  const trainerPersonIds = input.trainerPersonIds ??
    after?.trainersList.map((x) => x.personId) ?? fallback.trainerPersonIds;

  let previous = after;
  if (!previous) {
    for (const event of events) {
      if (
        !event.isCancelled &&
        eq(new Date(event.since), since, 'day') &&
        new Date(event.until) <= since &&
        event.trainersList.some((x) => trainerPersonIds.includes(x.personId)) &&
        (!previous || new Date(event.since) > new Date(previous.since))
      ) {
        previous = event;
      }
    }
  }
  const explicitLocation = input.locationId !== undefined || input.locationText !== undefined;
  const locationId = explicitLocation ? input.locationId : previous?.location?.id;
  const locationText = explicitLocation
    ? input.locationText ?? ''
    : previous?.location?.id ? '' : previous?.locationText ?? '';

  return {
    name: '',
    type,
    locationId: locationText ? 'other' : locationId ?? 'none',
    locationText,
    capacity: type === 'LESSON' ? 1 : 0,
    capacityUnit: type === 'LESSON' ? 'REGISTRATIONS' : 'PEOPLE',
    isVisible: true,
    isPublic: false,
    hasPublicDetails: false,
    isLocked: fallback.isLocked,
    enableNotes: false,
    instances: [{
      itemId: null,
      since: since.toISOString(),
      until: until.toISOString(),
      isCancelled: false,
    }],
    trainers: trainerPersonIds.map((personId) => ({ personId, lessonsOffered: 0 })),
    cohorts: [],
    registrations: [],
  };
}
