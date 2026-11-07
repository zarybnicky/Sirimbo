import { atom } from 'jotai';
import { atomWithStorage, selectAtom } from 'jotai/utils';
import type { CalendarViewKey } from './CalendarViews';
import type { CalendarGroupBy } from './useCalendarData';
import type { EventType } from '@/graphql';
import type { CreateEventDefaults } from './eventDefaults';
import type {
  CalendarClubEvent,
  DragAction,
  DragDirection,
  InteractionInfo,
  ResourceRef,
} from './types';

export type CalendarEventConflict = {
  id: string;
  personName: string | null;
  otherEventName: string | undefined;
  otherSince: string;
  otherUntil: string;
};

export const timeslotsAtom = atom(4);
export const stepAtom = atom(15);
export const minTimeAtom = atom(new Date(1972, 0, 1, 7, 0, 0));
export const maxTimeAtom = atom(new Date(1972, 0, 1, 23, 59, 59, 999));
export const focusedTimeAtom = atom(new Date(1972, 0, 1, 16, 0, 0));

export const calendarPreferencesAtom = atomWithStorage<{
  v?: CalendarViewKey;
  groupBy?: CalendarGroupBy;
}>('calendarPreferences', {});

export const isDraggingAtom = atom(false);

export type DragSubject = null | {
  action: DragAction;
  event?: CalendarClubEvent | null;
  direction?: DragDirection | null;
};
export const dragSubjectAtom = atom<DragSubject>(null);
export type ExternalDragSubject = {
  id: string;
  durationMinutes: number;
  resource?: ResourceRef;
};
export const externalDragDataType = 'application/x-tkolymp-calendar-item';
export const externalDragSubjectAtom = atom<ExternalDragSubject | null>(null);

export function readExternalDragSubject(
  dataTransfer: Pick<DataTransfer, 'getData'> | null,
  preview: ExternalDragSubject | null,
) {
  const serialized = dataTransfer?.getData(externalDragDataType);
  if (!serialized) return preview;

  try {
    const subject = JSON.parse(serialized) as ExternalDragSubject;
    return subject.id && subject.durationMinutes > 0 ? subject : preview;
  } catch {
    return preview;
  }
}
export const dragListenersAtom = atom<{
  onMove?: (event: CalendarClubEvent, info: InteractionInfo) => void;
  onResize?: (event: CalendarClubEvent, info: InteractionInfo) => void;
  onRemove?: (event: CalendarClubEvent) => void | Promise<void>;
  onDropFromOutside?: (
    subject: ExternalDragSubject,
    info: InteractionInfo,
  ) => void | Promise<void>;
  onCreate?: (defaults: CreateEventDefaults) => void;
  onDrillDown?: (date: Date) => void;
}>({});

export const calendarConflictsAtom = atom<Record<string, CalendarEventConflict[]>>({});

const emptyConflicts: CalendarEventConflict[] = [];

export const calendarConflictsFor = (instanceId: string | null | undefined) =>
  selectAtom(calendarConflictsAtom, (conflicts) => {
    if (!instanceId) return emptyConflicts;
    return conflicts[instanceId] ?? emptyConflicts;
  });

export const eventTypes = [
  'LESSON',
  'GROUP',
  'RESERVATION',
  'CAMP',
  'HOLIDAY',
] as const satisfies readonly EventType[];
