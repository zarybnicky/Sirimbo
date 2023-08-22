import classNames from 'classnames';
import React from 'react';
import { DnDContext, DragDirection } from './DnDContext';
import { ceil, diff } from './localizer';
import { SelectionContext } from './SelectContext';
import { CalendarEvent } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@app/ui/popover';
import { formatDefaultEventName, formatFullName, formatRegistrant } from '@app/ui/format-name';
import { Users, User, Clock, Calendar, Link as LinkIcon } from 'lucide-react';
import Link from 'next/link';
import { fullDateFormatter, shortTimeFormatter } from '@app/ui/format-date';

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
    <Popover>
      <PopoverTrigger asChild>
        <div
          tabIndex={0}
          style={style}
          onClick={onClick}
          onMouseDown={onTouchOrMouse}
          onTouchStart={onTouchOrMouse}
          className={classNames(className, {
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
      </PopoverTrigger>

      <PopoverContent>
        <div className="flex items-center gap-2">
          <Link href={`/akce/${event.id}`} className="text-accent-11 underline">
            {formatDefaultEventName(event.event!)}
            <LinkIcon />
          </Link>
        </div>
        <div className="flex items-center gap-2">
          <Calendar className="w-6 h-6 text-red-500" />
          {fullDateFormatter.formatRange(event.start, event.end)}
        </div>

        {event.event?.type === 'LESSON' && (
          <div className="flex items-center gap-2">
            <Clock className="w-6 h-6 text-red-500" />
            {shortTimeFormatter.formatRange(event.start, event.end)}
          </div>
        )}

        <div className="flex items-center gap-2">
          <User className="w-6 h-6 text-red-500" />
          {event.event?.eventTrainersList.map((x) => formatFullName(x.person)).join(', ')}
        </div>

        <div className="flex items-center gap-2">
          <Users className="w-6 h-6 text-red-500" />
          <span>
            {event.event?.eventRegistrationsList.length === 0 ? (
              <div>VOLNÁ</div>
            ) : (
              event.event?.eventRegistrationsList.map((reg) => <div key={reg.id}>{formatRegistrant(reg)}</div>)
            )}
          </span>
        </div>
      </PopoverContent>
    </Popover>
  )
}

export default EventCell
