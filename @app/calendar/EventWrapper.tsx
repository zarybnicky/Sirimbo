import React from 'react';
import clsx from 'clsx';
import { DnDContext, DragDirection } from './DnDContext';
import { CalendarEvent } from './types';

const EventWrapper: React.FC<{
  children: React.ReactElement;
  event: CalendarEvent;
  resourceId?: number;
  type: 'date' | 'time';
  continuesPrior?: boolean;
  continuesAfter?: boolean;
}> = ({ children, event, resourceId, type, continuesPrior, continuesAfter }) => {
  const { draggable } = React.useContext(DnDContext);

  const newProps = {
    onMouseDown(e: React.MouseEvent<HTMLDivElement>) {
      if (e.button !== 0) return;
      if (event.isDraggable === false) return
      // hack: because of the way the anchors are arranged in the DOM, resize
      // anchor events will bubble up to the move anchor listener. Don't start
      // move operations when we're on a resize anchor.
      if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
        event.sourceResource = resourceId;
        draggable.onBeginAction(event, 'move');
      }
    },
    onTouchStart(e: React.MouseEvent<HTMLDivElement>) {
      if (e.button !== 0) return;
      if (event.isDraggable === false) return;
      if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
        event.sourceResource = resourceId;
        draggable.onBeginAction(event, 'move');
      }
    },
    children: children.props.children,
    className: clsx(children.props.className, {
      'rbc-dragged-event': draggable.dragAndDropAction.current.interacting && draggable.dragAndDropAction.current.event === event,
    }),
  };


  if (event.isResizable !== false) {
    const renderAnchor = (direction: DragDirection) => (
      <div
        className={
          ['UP', 'DOWN'].includes(direction)
            ? `rbc-resize-ns-anchor`
            : `rbc-resize-ew-anchor`
        }
        onMouseDown={(e) => {
          if (e.button !== 0) return;
          draggable.onBeginAction(event, 'resize', direction);
        }}
      >
        <div
          className={
            ['UP', 'DOWN'].includes(direction)
              ? `rbc-resize-ns-icon`
              : `rbc-resize-ew-icon`
          }
        />
      </div>
    );
    newProps.children = (
      <div className="rbc-resizable">
        {!continuesPrior && renderAnchor(type === 'date' ? 'LEFT' : 'UP')}
        {children.props.children}
        {!continuesAfter && renderAnchor(type === 'date' ? 'RIGHT' : 'DOWN')}
      </div>
    );
  }

  return React.cloneElement(children, newProps);
};

export default EventWrapper;
