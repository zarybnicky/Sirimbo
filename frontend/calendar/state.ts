import { atom } from 'jotai'
import { atomWithDefault } from 'jotai/utils'
import { CalendarEvent, DragAction, DragDirection, InteractionInfo, SlotInfo } from './types';

export const timeslotsAtom = atom(4);
export const stepAtom = atom(15);
export const minTimeAtom = atom(new Date(1972, 0, 1, 7, 0, 0));
export const maxTimeAtom = atom(new Date(1972, 0, 1, 23, 59, 59, 999));
export const focusedTimeAtom = atom(new Date(1972, 0, 1, 16, 0, 0));
export const dateAtom = atomWithDefault(() => new Date())

export const showMineAtom = atom(false);

export const isDraggingAtom = atom(false);
export const dragSubjectAtom = atom<Record<string, never> | {
  action: DragAction;
  event?: CalendarEvent | null;
  direction?: DragDirection | null;
}>({});
export const groupByAtom = atom<'none' | 'trainer' | 'room'>('trainer');

export const dragListenersAtom = atom({
  onMove: (_e: CalendarEvent, _info: InteractionInfo) => {},
  onResize: (_e: CalendarEvent, _info: InteractionInfo) => {},
  onSelectSlot: (_slot: SlotInfo) => {},
  onDrillDown: (_date: Date) => {},
});

// declare function onDropFromOutside(info: {
//   start: Date;
//   end: Date;
//   allDay: boolean;
//   resourceId?: string;
// }): void;

// declare function dragFromOutsideItem(): CalendarEvent | undefined;
