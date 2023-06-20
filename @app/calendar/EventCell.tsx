import clsx from 'clsx';
import React from 'react';
import { DnDContext } from './DnDContext';
import { ceil, diff } from './localizer';
import { SelectionContext } from './SelectContext';
import { CalendarEvent } from './types';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
  isAllDay?: boolean;
  continuesPrior: boolean;
  continuesAfter: boolean;
  resourceId?: number;
}

const EventCell = ({
  style,
  className,
  event,
  isAllDay,
  continuesPrior,
  continuesAfter,
  resourceId,
}: EventCellProps) => {
  const draggable = React.useContext(DnDContext);
  const { onSelectEvent } = React.useContext(SelectionContext);
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  return (
    <div
      tabIndex={0}
      style={style}
      onClick={() => onSelectEvent(event)}
      onMouseDown={(e) => {
        if (e.button !== 0) return;
        if (event.isDraggable === false) return
        // hack: because of the way the anchors are arranged in the DOM, resize
        // anchor events will bubble up to the move anchor listener. Don't start
        // move operations when we're on a resize anchor.
        if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
          event.sourceResource = resourceId;
          draggable.onBeginAction(event, 'move');
        }
      }}
      onTouchStart={(e) => {
        if (!isDraggable) return;
        if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
          event.sourceResource = resourceId;
          draggable.onBeginAction(event, 'move');
        }
      }}
      className={clsx({
        'rbc-event': true,
        [className ?? '']: true,
        // TODO: 'rbc-selected': selected,
        'rbc-resizable': isResizable,
        'rbc-event-allday': isAllDay || event.allDay || diff(event.start, ceil(event.end, 'day'), 'day') > 1,
        'rbc-event-continues-prior': continuesPrior,
        'rbc-event-continues-after': continuesAfter,
        'rbc-drag-preview': event.__isPreview,
        'rbc-nondraggable': event.isDraggable === false,
        'cursor-grab': event.isDraggable !== false,
        'rbc-dragged-event': draggable.stateRef.current.interacting && draggable.stateRef.current.event === event,
      })}
    >
      {!continuesPrior && isResizable && (
        <div
          className="rbc-resize-ew-anchor"
          onMouseDown={(e) => {
            if (e.button !== 0) return;
            draggable.onBeginAction(event, 'resize', "LEFT");
          }}
        >
          <div className="rbc-resize-ew-icon" />
        </div>
      )}

      <div className="rbc-event-content">
        {event.title || '-'}
      </div>

      {!continuesAfter && isResizable && (
        <div
          className="rbc-resize-ew-anchor"
          onMouseDown={(e) => {
            if (e.button !== 0) return;
            draggable.onBeginAction(event, 'resize', "RIGHT");
          }}
        >
          <div className="rbc-resize-ew-icon" />
        </div>
      )}
    </div>
  )
}

export default EventCell
