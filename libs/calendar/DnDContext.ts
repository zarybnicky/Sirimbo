import React from 'react';
import { CalendarEvent } from './types';

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export type DnDState = {
  interacting: boolean;
  action?: DragAction | null;
  event?: CalendarEvent | null;
  direction?: DragDirection | null;
};

declare function onDropFromOutside(info: {
  start: Date;
  end: Date;
  allDay: boolean;
  resourceId?: number;
}): void;

export type DnDContextType = {
  onStart: () => void;
  onEnd: (info?: { start: Date; end: Date; resourceId?: number; isAllDay?: boolean }) => void;
  onBeginAction: (event: CalendarEvent, action: DragAction, direction?: DragDirection) => void;
  onDropFromOutside?: typeof onDropFromOutside;
  dragFromOutsideItem: () => CalendarEvent | undefined;
  stateRef: React.MutableRefObject<DnDState>;
};

export const DnDContext = React.createContext<DnDContextType>(
  null as any as DnDContextType,
);

export const DndProvider = ({ setIsDragging, children }: {
  children: React.ReactNode;
  setIsDragging: React.Dispatch<React.SetStateAction<boolean>>;
}) => {
  const stateRef = React.useRef<DnDState>({ interacting: false });
  const context = React.useMemo<DnDContextType>(() => ({
    onBeginAction(event: CalendarEvent, action, direction) {
      stateRef.current = { action, event, interacting: true, direction };
    },
    onStart() {
      stateRef.current = { ...stateRef.current, interacting: true };
      setIsDragging(true);
    },
    onEnd(interactionInfo) {
      const { event, action } = stateRef.current;
      stateRef.current = { action: null, event: null, interacting: false, direction: null };
      setIsDragging(false);

      if (!action || !event || !interactionInfo) return
      if (action === 'move') {
        console.log('onMove', event, interactionInfo);
        // TODO: onDrop
      }
      if (action === 'resize') {
        console.log('onResize', event, interactionInfo)
        // TODO: onResize
      }
    },
    onDropFromOutside(details) {
      setIsDragging(false);
      console.log('onDropFromOutside', details)
      // TODO: onDrop
    },
    dragFromOutsideItem() {
      console.log('dragFromOutside', stateRef.current);
      // TODO: dragFromOutside
      return undefined
    },
    stateRef,
  }), [setIsDragging]);

  return React.createElement(DnDContext.Provider, { value: context }, children);
};
