import React from 'react';
import clsx from 'clsx';
import { DnDContext, DragDirection } from './DnDContext';
import { Event } from './types';

const EventWrapper: React.FC<{
  children: React.ReactElement;
  event: Event;
  resourceId?: number;
  type: 'date' | 'time';
  continuesPrior?: boolean;
  continuesAfter?: boolean;
}> = ({ children, event, resourceId, type, continuesPrior, continuesAfter }) => {
  const { draggable } = React.useContext(DnDContext);

  if (event.__isPreview) {
    return React.cloneElement(children, {
      className: clsx(children.props.className, 'rbc-addons-dnd-drag-preview'),
    });
  }

  if (event.isDraggable === false) {
    return children;
  }

  const newProps = {
    onMouseDown(e: React.MouseEvent<HTMLDivElement>) {
      if (e.button !== 0) return;
      // hack: because of the way the anchors are arranged in the DOM, resize
      // anchor events will bubble up to the move anchor listener. Don't start
      // move operations when we're on a resize anchor.
      if (!e.currentTarget.getAttribute('class')?.includes('rbc-addons-dnd-resize')) {
        event.sourceResource = resourceId;
        draggable.onBeginAction(event, 'move');
      }
    },
    onTouchStart(e: React.MouseEvent<HTMLDivElement>) {
      if (e.button !== 0) return;
      if (!e.currentTarget.getAttribute('class')?.includes('rbc-addons-dnd-resize')) {
        event.sourceResource = resourceId;
        draggable.onBeginAction(event, 'move');
      }
    },
    className: '',
    children: children.props.children,
  };

  if (event.isResizable !== false) {
    const renderAnchor = (direction: DragDirection) => (
      <div
        className={
          ['UP', 'DOWN'].includes(direction)
            ? `rbc-addons-dnd-resize-ns-anchor`
            : `rbc-addons-dnd-resize-ew-anchor`
        }
        onMouseDown={(e) => {
          if (e.button !== 0) return;
          draggable.onBeginAction(event, 'resize', direction);
        }}
      >
        <div
          className={
            ['UP', 'DOWN'].includes(direction)
              ? `rbc-addons-dnd-resize-ns-icon`
              : `rbc-addons-dnd-resize-ew-icon`
          }
        />
      </div>
    );
    newProps.children = (
      <div className="rbc-addons-dnd-resizable">
        {!continuesPrior && renderAnchor(type === 'date' ? 'LEFT' : 'UP')}
        {children.props.children}
        {!continuesAfter && renderAnchor(type === 'date' ? 'RIGHT' : 'DOWN')}
      </div>
    );
  }

  if (
    draggable.dragAndDropAction.current.interacting &&
    draggable.dragAndDropAction.current.event === event
  ) {
    newProps.className = clsx(children.props.className, 'rbc-addons-dnd-dragged-event');
  }

  return React.cloneElement(children, newProps);
};

export default EventWrapper;
