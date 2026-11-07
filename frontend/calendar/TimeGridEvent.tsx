import React, { useCallback } from 'react';
import type { TimeSlotMetrics } from './TimeSlotMetrics';
import type { CalendarClubEvent, DragDirection } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { type DragSubject, dragSubjectAtom, isDraggingAtom } from './state';
import { cn } from '@/lib/cn';
import { selectAtom } from 'jotai/utils';
import { formatEventName, shortTimeFormatter } from '@/ui/format';
import { useTenantConfig } from '@/lib/auth';
import { EventConflictsBadge } from '@/calendar/EventConflictsBadge';

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
  event: CalendarClubEvent;
  isBackgroundEvent?: boolean;
  slotMetrics: TimeSlotMetrics;
};

function TimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
}: TimeGridEventProps) {
  const { useTrainerInitials } = useTenantConfig();
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
        setDragSubject({ action: 'move', event });
      }
    },
    [setDragSubject, event, isDraggable, isResizable],
  );

  let title = formatEventName(event.instance) || '-';
  if (title === 'Lekce') title = event.instance.registrationInfo?.canRegister ? 'Volná' : '-';

  let label =
    startsBeforeDay && startsAfterDay
      ? 'Celý den'
      : startsBeforeDay
        ? ` – ${shortTimeFormatter.format(event.end)}`
        : startsAfterDay
          ? `${shortTimeFormatter.format(event.start)} – `
          : shortTimeFormatter.format(event.start);
  for (const trainer of event.instance.eventTrainersList ?? []) {
    const trainerLabel = formatTrainerLabel(trainer.person?.name, useTrainerInitials);
    if (trainerLabel) label += `, ${trainerLabel}`;
  }
  const location = event.instance.location?.name || event.instance.locationText || '';
  if (location) label += `, ${location}`;

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
          'absolute overflow-hidden max-h-full min-h-5 border border-b-transparent',
          {
            'w-full h-full': isResizable,
            'empty-event': event.instance.registrationInfo?.canRegister,
            'saturate-50 opacity-75': !event.instance.registrationInfo?.registrations &&
              !event.instance.registrationInfo?.registrationOpen && !event.instance.name,
            'is-group': event.instance.type === 'GROUP',
            'opacity-75': isBackgroundEvent,
            'rbc-drag-preview': event.__isPreview,
            'rounded-t-none': continuesPrior,
            'rounded-b-none': continuesAfter,
            'rbc-dragged-event': isDragging && currentDragSubject && !event.__isPreview,
            'pl-3': !!event.instance.targetCohortsList?.length,
          },
        )}
      >
        <EventConflictsBadge
          eventId={event.id}
          className="absolute right-1 top-1 text-accent-11 drop-shadow-sm"
        />
        {!!event.instance.targetCohortsList?.length && (
          <div className="absolute overflow-hidden opacity-80 border-r border-neutral-10/50 shadow-xs inset-y-0 left-0 flex flex-col">
            {event.instance.targetCohortsList
              .flatMap((x) => x.cohort?.colorRgb || [])
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
          className={cn(
            'rbc-event-content w-full wrap-break-word leading-none min-h-[1em]',
            {
              'line-through': event.instance.isCancelled,
            },
          )}
        >
          {title}
        </div>
        <div className="block truncate text-[80%] pr-1.25 w-auto">{label}</div>

        {!continuesPrior && isResizable && (
          <div
            className="absolute bottom-0 opacity-0 group-hover:opacity-100 cursor-s-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="DOWN"
          />
        )}
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary event={event.instance} />
      </PopoverContent>
    </Popover>
  );
}

export default TimeGridEvent;
