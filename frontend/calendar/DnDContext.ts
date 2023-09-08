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

export type InteractionInfo = {
  start: Date;
  end: Date;
  resourceId?: string;
  isAllDay?: boolean;
};

declare function onDropFromOutside(info: {
  start: Date;
  end: Date;
  allDay: boolean;
  resourceId?: string;
}): void;

export type DnDContextType = {
  onStart: () => void;
  onEnd: (info?: { start: Date; end: Date; resourceId?: string; isAllDay?: boolean }) => void;
  onBeginAction: (event: CalendarEvent, action: DragAction, direction?: DragDirection) => void;
  onDropFromOutside?: typeof onDropFromOutside;
  dragFromOutsideItem: () => CalendarEvent | undefined;
  stateRef: React.MutableRefObject<DnDState>;
};

export const DnDContext = React.createContext<DnDContextType>(
  null as any as DnDContextType,
);

export const DndProvider = ({ onMove, onResize, setIsDragging, children }: {
  children: React.ReactNode;
  setIsDragging: React.Dispatch<React.SetStateAction<boolean>>;
  onMove: (e: CalendarEvent, info: InteractionInfo) => void;
  onResize: (e: CalendarEvent, info: InteractionInfo) => void;
}) => {
  const stateRef = React.useRef<DnDState>({ interacting: false });
  const context = React.useMemo<DnDContextType>(() => ({
    stateRef,
    onBeginAction(event: CalendarEvent, action, direction) {
      stateRef.current = { action, event, interacting: true, direction };
    },

    onStart() {
      stateRef.current = { ...stateRef.current, interacting: true };
      setIsDragging(true);
      document.querySelectorAll<HTMLElement>('.rbc-time-content').forEach(x => x.style.overflowY = 'hidden');
      document.querySelectorAll<HTMLElement>('body').forEach(x => x.style.overflowY = 'hidden');
    },

    onEnd(interactionInfo) {
      const { event, action } = stateRef.current;
      stateRef.current = { action: null, event: null, interacting: false, direction: null };
      setIsDragging(false);
      document.querySelectorAll<HTMLElement>('.rbc-time-content').forEach(x => x.style.overflowY = '');
      document.querySelectorAll<HTMLElement>('body').forEach(x => x.style.overflowY = '');

      if (!action || !event || !interactionInfo) return
      if (action === 'move') {
        onMove(event, interactionInfo);
      }
      if (action === 'resize') {
        onResize(event, interactionInfo);
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
  }), [setIsDragging]);

  return React.createElement(DnDContext.Provider, { value: context }, children);
};
