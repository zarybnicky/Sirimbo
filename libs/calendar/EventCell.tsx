import clsx from 'clsx';
import React from 'react';
import { DnDContext, DragDirection } from './DnDContext';
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

  const onClick = React.useCallback(() => {
    onSelectEvent(event)
  }, [event, onSelectEvent]);

  const onTouchOrMouse = React.useCallback((e: React.TouchEvent | React.MouseEvent) => {
    if (!!(e as React.MouseEvent).button) {
      return;
    }
    const resizeDirection = (e.target as HTMLElement).dataset.resize
    if (isResizable && resizeDirection) {
      draggable.onBeginAction(event, 'resize', resizeDirection as DragDirection);
    } else if (isDraggable) {
      event.sourceResource = resourceId;
      draggable.onBeginAction(event, 'move');
    }
  }, [draggable, event, isDraggable, isResizable, resourceId]);

  return (
    <div
      tabIndex={0}
      style={style}
      onClick={onClick}
      onMouseDown={onTouchOrMouse}
      onTouchStart={onTouchOrMouse}
      className={clsx(className, {
        'rbc-event group transition-opacity': true,
        // TODO: 'rbc-selected': selected,
        'rbc-resizable': isResizable,
        'rbc-event-allday': isAllDay || event.allDay || diff(event.start, ceil(event.end, 'day'), 'day') > 1,
        'rounded-l-none': continuesPrior,
        'rounded-r-none': continuesAfter,
        'cursor-grab': event.isDraggable !== false,
        'rbc-nondraggable': event.isDraggable === false,
        'rbc-drag-preview': event.__isPreview,
        'rbc-dragged-event': draggable.stateRef.current.event === event,
      })}
    >
      {!continuesPrior && isResizable && (
        <div
          className="absolute left-0 opacity-0 group-hover:opacity-100 cursor-w-resize h-3 top-2 mx-auto border-l-4 border-double"
          data-resize="LEFT"
        />
      )}

      <div className="rbc-event-content">
        {event.title || '-'}
      </div>

      {!continuesAfter && isResizable && (
        <div
          className="absolute right-0 opacity-0 group-hover:opacity-100 cursor-e-resize h-3 top-2 mx-auto border-l-4 border-double"
          data-resize="RIGHT"
        />
      )}
    </div>
  )
}

export default EventCell