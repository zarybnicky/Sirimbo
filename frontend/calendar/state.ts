import { atom } from 'jotai';
import { selectAtom } from 'jotai/utils';
import type { EventType } from '@/graphql';
import type {
  CalendarInstanceEvent,
  DragAction,
  DragDirection,
  InteractionInfo,
  SlotInfo,
} from './types';

export type CalendarInstanceConflict = {
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

export const isDraggingAtom = atom(false);

export type DragSubject = null | {
  action: DragAction;
  event?: CalendarInstanceEvent | null;
  direction?: DragDirection | null;
};
export const dragSubjectAtom = atom<DragSubject>(null);
export type ExternalDragSubject = {
  id: string;
  durationMinutes: number;
  resourceId?: string;
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
  onMove?: (event: CalendarInstanceEvent, info: InteractionInfo) => void;
  onResize?: (event: CalendarInstanceEvent, info: InteractionInfo) => void;
  onRemove?: (event: CalendarInstanceEvent) => void | Promise<void>;
  onDropFromOutside?: (
    subject: ExternalDragSubject,
    info: InteractionInfo,
  ) => void | Promise<void>;
  onSelectSlot?: (slot: SlotInfo) => void;
  onDrillDown?: (date: Date) => void;
}>({});

export const calendarConflictsAtom = atom<Record<string, CalendarInstanceConflict[]>>({});

const emptyConflicts: CalendarInstanceConflict[] = [];

export const calendarConflictsFor = (instanceId: string | null | undefined) =>
  selectAtom(calendarConflictsAtom, (conflicts) => {
    if (!instanceId) return emptyConflicts;
    return conflicts[instanceId] ?? emptyConflicts;
  });

// declare function onDropFromOutside(info: {
//   start: Date;
//   end: Date;
//   resourceId?: string;
// }): void;

// declare function dragFromOutsideItem(): CalendarEvent | undefined;

const storage = {
  getItem(key: string): string | null {
    return typeof localStorage === 'undefined' ? null : localStorage.getItem(key);
  },
  setItem(key: string, value: string | null) {
    if (value) {
      localStorage.setItem(key, value);
    } else {
      localStorage.removeItem(key);
    }
  },
};

export const trainerIdsFilterAtom = atom<string[]>([]);
export const participantIdsFilterAtom = atom<string[]>([]);
export const eventTypes = [
  'LESSON',
  'GROUP',
  'RESERVATION',
  'CAMP',
  'HOLIDAY',
] as const satisfies readonly EventType[];
export const eventTypesFilterAtom = atom<EventType[]>([...eventTypes]);

const baseGroupByAtom = atom(
  ((storage.getItem('groupBy') as any) || 'trainer') as 'none' | 'trainer' | 'room',
);

export const groupByAtom = atom(
  (get) => get(baseGroupByAtom),
  (get, set, nextValue: 'none' | 'trainer' | 'room') => {
    if (get(baseGroupByAtom) !== nextValue) {
      set(baseGroupByAtom, nextValue);
      storage.setItem('groupBy', nextValue);
    }
  },
);
