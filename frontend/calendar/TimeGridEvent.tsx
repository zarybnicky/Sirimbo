import { shortTimeIntl } from './localizer';
import React, { useCallback } from 'react';
import type { TimeSlotMetrics } from './TimeSlotMetrics';
import type { CalendarEvent, DragDirection, Resource } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { type DragSubject, dragSubjectAtom, isDraggingAtom } from './state';
import { cn } from '@/lib/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';
import { isTruthy } from '@/lib/truthyFilter';
import { tenantConfigAtom } from '@/ui/state/auth';
import { ConflictsInstanceBadge } from '@/calendar/ConflictsInstanceBadge';

function formatTrainerLabel(name: string | undefined, useInitials: boolean): string {
  if (!name) return '';
  if (!useInitials) {
    return name;
  }

  const sanitized = name
    .replaceAll(/\b(Mgr\.|Ing\.|Bc\.)\s*/g, '')
    .normalize('NFKD')
    .replaceAll(/[^A-Z]/g, '');

  return sanitized || name;
}

type TimeGridEventProps = {
  style: {
    top: number;
    width: number;
    height: number;
    xOffset: number;
  };
  className?: string;
  event: CalendarEvent;
  isBackgroundEvent?: boolean;
  slotMetrics: TimeSlotMetrics;
  resource?: Resource;
};

function TimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
  resource,
}: TimeGridEventProps) {
  const { useTrainerInitials } = useAtomValue(tenantConfigAtom);
  const isDragging = useAtomValue(isDraggingAtom);
  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback(
    (v: DragSubject) => (v?.event?.instance.id === event.instance.id ? v : null),
    [event],
  );
  const [currentDragSubject] = useAtom(selectAtom(dragSubjectAtom, getCurrentEvent));

  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const startsBeforeDay = slotMetrics.startsBeforeDay(event.start);
  const startsBefore = slotMetrics.startsBefore(event.start);
  const continuesPrior = startsBeforeDay || startsBefore;

  const startsAfterDay = slotMetrics.startsAfterDay(event.end);
  const startsAfter = slotMetrics.startsAfter(event.end);
  const continuesAfter = startsAfterDay || startsAfter;

  const onTouchOrMouse = React.useCallback(
    (e: React.TouchEvent | React.MouseEvent) => {
      if ((e as React.MouseEvent).button) {
        return;
      }
      const resizeDirection = (e.target as HTMLElement).dataset.resize;
      if (isResizable && resizeDirection) {
        setDragSubject({
          action: 'resize',
          event,
          direction: resizeDirection as DragDirection,
        });
      } else if (isDraggable) {
        setDragSubject({ action: 'move', event: { ...event, sourceResource: resource } });
      }
    },
    [setDragSubject, event, isDraggable, isResizable, resource],
  );

  const title =
    event.instance.name || (event.event ? formatDefaultEventName(event.event) : '-');
  const label = React.useMemo(() => {
    let label = '';
    if (startsBeforeDay && startsAfterDay) {
      label += 'Celý den';
    } else if (startsBeforeDay) {
      label += ` – ${shortTimeIntl.format(event.end)}`;
    } else if (startsAfterDay) {
      label += `${shortTimeIntl.format(event.start)} – `;
    } else {
      label += shortTimeIntl.format(event.start);
    }

    const trainers = event.instance.trainersList ?? [];
    for (const trainer of trainers) {
      const trainerLabel = formatTrainerLabel(trainer.person?.name, useTrainerInitials);
      if (!trainerLabel) continue;
      label += `, ${trainerLabel}`;
    }

    const location = event.instance.location?.name || event.instance.locationText || '';
    if (location) label += `, ${location}`;

    return label;
  }, [
    event.end,
    event.instance.location?.name,
    event.instance.locationText,
    event.instance.trainersList,
    event.start,
    slotMetrics,
    useTrainerInitials,
  ]);

  return (
    <Popover modal>
      <PopoverTrigger
        onMouseDown={onTouchOrMouse}
        onTouchStart={onTouchOrMouse}
        style={{
          top: `${style.top}%`,
          width: `${style.width}%`,
          height: `${style.height}%`,
          left: `${style.xOffset}%`,
        }}
        title={label + ' ' + title}
        className={cn(
          className,
          'rbc-event group transition-opacity',
          'absolute overflow-hidden max-h-full min-h-[20px] border border-b-transparent',
          {
            'w-full h-full': isResizable,
            'empty-event': event.event.eventRegistrations.totalCount === 0,
            'is-group': event.instance.type === 'GROUP',
            'opacity-75': isBackgroundEvent,
            'rbc-drag-preview': event.__isPreview,
            'rounded-t-none': continuesPrior,
            'rounded-b-none': continuesAfter,
            'rbc-dragged-event': isDragging && currentDragSubject && !event.__isPreview,
            'pl-3': event.event.eventTargetCohortsList.length > 0,
          },
        )}
      >
        <ConflictsInstanceBadge
          instanceId={event.instance.id}
          className="absolute right-1 top-1 text-accent-11 drop-shadow"
        />
        {event.event.eventTargetCohortsList.length > 0 && (
          <div className="absolute overflow-hidden opacity-80 border-r border-neutral-10/50 shadow-sm inset-y-0 left-0 flex flex-col">
            {event.event.eventTargetCohortsList
              .map((x) => x.cohort?.colorRgb)
              .filter(isTruthy)
              .map((color) => (
                <div
                  key={color}
                  className="flex-1 w-2"
                  style={{ backgroundColor: color }}
                />
              ))}
          </div>
        )}
        {!continuesPrior && isResizable && (
          <div
            className="absolute top-0 opacity-0 group-hover:opacity-100 cursor-n-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="UP"
          />
        )}

        <div
          className={cn('rbc-event-content w-full break-words leading-none min-h-[1em]', {
            'line-through': event.instance.isCancelled,
          })}
        >
          {title}
        </div>
        <div className="block truncate text-[80%] pr-[5px] w-auto">{label}</div>

        {!continuesPrior && isResizable && (
          <div
            className="absolute bottom-0 opacity-0 group-hover:opacity-100 cursor-s-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="DOWN"
          />
        )}
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary offsetButtons event={event.event} instance={event.instance} />
      </PopoverContent>
    </Popover>
  );
}

export default TimeGridEvent;
