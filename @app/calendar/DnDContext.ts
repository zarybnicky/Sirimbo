import React from 'react'
import { DragAction, DragDirection, Event } from './utils/constants';

export type DnDState = {
  interacting: boolean;
  action?: DragAction | null;
  event?: Event | null;
  direction?: DragDirection | null;
};

declare function onBeginAction(event: Event, action: 'move', direction?: DragDirection): void;
declare function onBeginAction(event: Event, action: 'resize', direction: DragDirection): void;

declare function onDropFromOutside(info: { start: Date, end: Date, allDay: boolean, resource?: number }): void;

export type DnDContextType = {
  draggable: {
    onStart: () => void;
    onEnd: (info: null | { start: Date, end: Date, resourceId: number, isAllDay: boolean }) => void;
    onBeginAction: typeof onBeginAction;
    onDropFromOutside: typeof onDropFromOutside;
    dragFromOutsideItem: unknown;
    dragAndDropAction: DnDState;
  };
}

export const DnDContext = React.createContext<DnDContextType>(null as any as DnDContextType);
