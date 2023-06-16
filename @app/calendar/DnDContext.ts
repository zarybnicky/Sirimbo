import React from 'react';
import { DragAction, DragDirection, Event } from './types';

export type DnDState = {
  interacting: boolean;
  action?: DragAction | null;
  event?: Event | null;
  direction?: DragDirection | null;
};

declare function onDropFromOutside(info: {
  start: Date;
  end: Date;
  allDay: boolean;
  resource?: number;
}): void;

export type DnDContextType = {
  draggable: {
    onStart: () => void;
    onEnd: (
      info: null | { start: Date; end: Date; resourceId: number; isAllDay: boolean },
    ) => void;
    onBeginAction: (event: Event, action: DragAction, direction?: DragDirection) => void;
    onDropFromOutside?: typeof onDropFromOutside;
    dragFromOutsideItem: unknown;
    dragAndDropAction: DnDState;
  };
};

export const DnDContext = React.createContext<DnDContextType>(
  null as any as DnDContextType,
);
