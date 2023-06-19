import React from 'react';
import { Event } from './types';

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

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
  resourceId?: number;
}): void;

export type DnDContextType = {
  draggable: {
    onStart: () => void;
    onEnd: (
      info: null | { start: Date; end: Date; resourceId?: number; isAllDay?: boolean },
    ) => void;
    onBeginAction: (event: Event, action: DragAction, direction?: DragDirection) => void;
    onDropFromOutside?: typeof onDropFromOutside;
    dragFromOutsideItem: () => Event | undefined;
    dragAndDropAction: React.MutableRefObject<DnDState>;
  };
};

export const DnDContext = React.createContext<DnDContextType>(
  null as any as DnDContextType,
);

export const DndProvider = ({ setIsDragging, children }: {
  children: React.ReactNode;
  setIsDragging: React.Dispatch<React.SetStateAction<boolean>>;
}) => {
  const state = React.useRef<DnDState>({ interacting: false });
  const context = React.useMemo<DnDContextType>(() => ({
    draggable: {
      onBeginAction(event: Event, action, direction) {
        state.current = { action, event, interacting: true, direction };
      },
      onStart() {
        state.current = { ...state.current, interacting: true };
        setIsDragging(true);
      },
      onEnd(interactionInfo) {
        const { event, action } = state.current;
        state.current = { action: null, event: null, interacting: false, direction: null };
        setIsDragging(false);

        if (!action || !event || !interactionInfo) return
        if (action === 'move') {
          // TODO: onDrop
        }
        if (action === 'resize') {
          // TODO: onResize
        }
      },
      onDropFromOutside({ start, end, allDay, resourceId }) {
        setIsDragging(false);
        // TODO: onDrop
      },
      dragFromOutsideItem() {
        // TODO: dragFromOutside
        return undefined
      },
      dragAndDropAction: state,
    },
  }), []);

  return React.createElement(DnDContext.Provider, { children, value: context });
};
