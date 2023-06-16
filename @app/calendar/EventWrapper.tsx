import React from 'react';
import clsx from 'clsx';
import { DnDContext } from './DnDContext';
import { DragDirection, Event } from './types';

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

  /* Event is not draggable, no need to wrap it */
  if (!draggable || event.isDraggable === false) {
    return children;
  }

  const handleStartDragging = (e: any) => {
    if (e.button !== 0) return;
    // hack: because of the way the anchors are arranged in the DOM, resize
    // anchor events will bubble up to the move anchor listener. Don't start
    // move operations when we're on a resize anchor.
    if (!e.target.getAttribute('class')?.includes('rbc-addons-dnd-resize')) {
      event.sourceResource = resourceId;
      draggable.onBeginAction(event, 'move');
    }
  };
  const newProps = {
    onMouseDown: handleStartDragging,
    onTouchStart: handleStartDragging,
    className: '',
    children: children.props.children,
  };

  /*
   * The resizability of events depends on whether they are
   * allDay events and how they are displayed.
   *
   * 1. If the event is being shown in an event row (because
   * it is an allDay event shown in the header row or because as
   * in month view the view is showing all events as rows) then we
   * allow east-west resizing.
   *
   * 2. Otherwise the event is being displayed
   * normally, we can drag it north-south to resize the times.
   *
   * props.children is the singular <Event> component.
   * BigCalendar positions the Event abolutely and we
   * need the anchors to be part of that positioning.
   * So we insert the anchors inside the Event's children
   * rather than wrap the Event here as the latter approach
   * would lose the positioning.
   */
  if (event.isResizable !== false) {
    // replace original event child with anchor-embellished child
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
        {continuesPrior ? null : renderAnchor(type === 'date' ? 'LEFT' : 'UP')}
        {children.props.children}
        {continuesAfter ? null : renderAnchor(type === 'date' ? 'RIGHT' : 'DOWN')}
      </div>
    );
  }

  if (
    draggable.dragAndDropAction.interacting && // if an event is being dragged right now
    draggable.dragAndDropAction.event === event // and it's the current event
  ) {
    // add a new class to it
    newProps.className = clsx(children.props.className, 'rbc-addons-dnd-dragged-event');
  }

  return React.cloneElement(children, newProps);
};

export default EventWrapper;
