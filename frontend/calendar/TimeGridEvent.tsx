import { shortTimeIntl } from './localizer';
import React, { useCallback } from 'react';
import type { TimeSlotMetrics } from './TimeSlotMetrics';
import type { CalendarEvent, DragDirection, Resource } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import {
  calendarConflictsFor,
  type DragSubject,
  dragSubjectAtom,
  isDraggingAtom,
} from './state';
import { cn } from '@/ui/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';
import { isTruthy } from '@/ui/truthyFilter';
import { AlertTriangle } from 'lucide-react';
import { tenantConfigAtom } from '@/ui/state/auth';

function formatTrainerLabel(name: string, useInitials: boolean): string {
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
    (v: DragSubject) => (v?.event === event ? v : null),
    [event],
  );
  const [currentDragSubject] = useAtom(selectAtom(dragSubjectAtom, getCurrentEvent));

  const conflictsAtom = React.useMemo(
    () => calendarConflictsFor(event.instance.id),
    [event.instance.id],
  );
  const conflicts = useAtomValue(conflictsAtom);
  const conflictNames = React.useMemo(
    () =>
      conflicts
        .map((conflict) => conflict.personName ?? conflict.fallbackName)
        .join(', '),
    [conflicts],
  );
  const conflictSummary = React.useMemo(
    () =>
      conflicts
        .map((conflict) => {
          const person = conflict.personName ?? conflict.fallbackName;
          const range = shortTimeIntl.formatRange(
            new Date(conflict.otherSince),
            new Date(conflict.otherUntil),
          );
          return `${person}: ${conflict.otherEventName} (${range})`;
        })
        .join(' • '),
    [conflicts],
  );

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

  const title = event.event ? formatDefaultEventName(event.event) : '-';
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
      const trainerLabel = formatTrainerLabel(
        trainer.person?.name ?? '',
        useTrainerInitials,
      );
      if (!trainerLabel) continue;
      label += `, ${trainerLabel}`;
    }

    const location = event.event.location?.name || event.event.locationText || '';
    if (location) label += `, ${location}`;

    return label;
  }, [
    event.end,
    event.event.location?.name,
    event.event.locationText,
    event.instance.trainersList,
    event.start,
    slotMetrics,
    useTrainerInitials,
  ]);

  const triggerTitle = React.useMemo(() => {
    const parts = [label, title];
    if (conflictSummary) {
      parts.push(`Kolize – ${conflictSummary}`);
    }
    return parts.filter(Boolean).join(': ');
  }, [label, title, conflictSummary]);

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
        title={triggerTitle}
        className={cn(className, {
          'rbc-event group transition-opacity': true,
          'rbc-resizable': isResizable,
          'empty-event': event.event.eventRegistrations.totalCount === 0,
          'is-group': event.event.type === 'GROUP',
          // TODO: 'rbc-selected': selected,
          'opacity-75': isBackgroundEvent,
          'rbc-drag-preview': event.__isPreview,
          'rounded-t-none': continuesPrior,
          'rounded-b-none': continuesAfter,
          'rbc-dragged-event': isDragging && currentDragSubject,
          'pl-3': event.event.eventTargetCohortsList.length > 0,
          relative: true,
        })}
      >
        {conflicts.length > 0 && (
          <>
            <div
              className="absolute right-1 top-1 text-accent-11 drop-shadow"
              aria-hidden
            >
              <AlertTriangle className="size-4" />
            </div>
            <span className="sr-only">Kolize: {conflictNames}</span>
          </>
        )}
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
          className={`rbc-event-content${event.instance.isCancelled ? ' line-through' : ''}`}
        >
          {title}
        </div>
        <div className="rbc-event-label">{label}</div>

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
