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
import { CompetitionEventContent } from '@/ui/Competitions';
import { Cake } from 'lucide-react';

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

function TimeGridEvent({ event, ...props }: TimeGridEventProps) {
  if (event.kind === 'birthday') return <BirthdayTimeGridEvent event={event} {...props} />;
  if (event.kind === 'competition') {
    return <CompetitionTimeGridEvent event={event} {...props} />;
  }
  return <InstanceTimeGridEvent event={event} {...props} />;
}

function InstanceTimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
  resource,
}: TimeGridEventProps & { event: Extract<CalendarEvent, { kind: 'event' }> }) {
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
  let label =
    startsBeforeDay && startsAfterDay
      ? 'Celý den'
      : startsBeforeDay
        ? ` – ${shortTimeIntl.format(event.end)}`
        : startsAfterDay
          ? `${shortTimeIntl.format(event.start)} – `
          : shortTimeIntl.format(event.start);
  for (const trainer of event.instance.trainersList ?? []) {
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

function CompetitionTimeGridEvent({
  style,
  className,
  event,
}: TimeGridEventProps & { event: Extract<CalendarEvent, { kind: 'competition' }> }) {
  return (
    <div
      style={{
        top: `${style.top}%`,
        width: `${style.width}%`,
        height: `${style.height}%`,
        left: `${style.xOffset}%`,
      }}
      title={event.title}
      className={cn(
        className,
        'competition-time-grid-event rbc-event absolute max-h-full min-h-[20px] overflow-y-auto border border-green-7 bg-green-3 p-2 text-green-12',
      )}
    >
      <CompetitionEventContent
        title={event.title}
        location={event.eventLocation}
        entries={event.items}
      />
    </div>
  );
}

function BirthdayTimeGridEvent({
  style,
  className,
  event,
}: TimeGridEventProps & { event: Extract<CalendarEvent, { kind: 'birthday' }> }) {
  return (
    <div
      style={{
        top: `${style.top}%`,
        width: `${style.width}%`,
        height: `${style.height}%`,
        left: `${style.xOffset}%`,
      }}
      title={`Narozeniny: ${event.person.name}`}
      className={cn(
        className,
        'rbc-event absolute flex max-h-full min-h-[20px] items-start gap-1 overflow-hidden border border-neutral-6 bg-neutral-2 p-2 text-neutral-12',
      )}
    >
      <Cake className="mt-0.5 size-3 shrink-0 text-accent-11" />
      <span className="truncate text-xs font-semibold">{event.person.name}</span>
    </div>
  );
}

export default TimeGridEvent;
