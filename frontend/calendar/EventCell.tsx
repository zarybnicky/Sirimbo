import React, { useCallback } from 'react';
import type { CalendarEvent, DragDirection } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtomValue, useSetAtom } from 'jotai';
import { type DragSubject, dragSubjectAtom, isDraggingAtom } from './state';
import { cn } from '@/lib/cn';
import { selectAtom } from 'jotai/utils';
import { formatEventName } from '@/ui/format';
import { EventConflictsBadge } from '@/calendar/EventConflictsBadge';
import { Cake } from 'lucide-react';
import { useAuth } from '@/lib/auth';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
  continuesPrior: boolean;
  continuesAfter: boolean;
};

function EventCell({ event, ...props }: EventCellProps) {
  if (event.kind === 'birthday') return <BirthdayEventCell event={event} {...props} />;
  if (event.kind === 'competition') return <CompetitionEventCell event={event} {...props} />;
  return <ClubEventCell event={event} {...props} />;
}

function BirthdayEventCell({
  style,
  className,
  event,
  continuesPrior,
  continuesAfter,
}: EventCellProps & { event: Extract<CalendarEvent, { kind: 'birthday' }> }) {
  return (
    <div
      style={style}
      title={`Narozeniny: ${event.person.name}`}
      className={cn(
        className,
        'rbc-event relative overflow-hidden border border-neutral-6 bg-neutral-2 text-neutral-12',
        {
          'rounded-l-none': continuesPrior,
          'rounded-r-none': continuesAfter,
        },
      )}
    >
      <div className="flex items-center gap-1 px-1 py-0.5 text-[0.68rem] font-semibold leading-tight">
        <Cake className="size-3 shrink-0 text-accent-11" />
        <span className="truncate">{event.person.name}</span>
      </div>
    </div>
  );
}

function CompetitionEventCell({
  style,
  className,
  event,
  continuesPrior,
  continuesAfter,
}: EventCellProps & { event: Extract<CalendarEvent, { kind: 'competition' }> }) {
  return (
    <div
      style={style}
      title={event.title}
      className={cn(
        className,
        'rbc-event relative overflow-hidden border border-green-7 bg-green-3 text-green-12',
        {
          'rounded-l-none': continuesPrior,
          'rounded-r-none': continuesAfter,
        },
      )}
    >
      <div className="rbc-event-content px-1 py-0.5 text-[0.68rem] font-semibold leading-tight">
        {event.title}
        {!!event.officials?.length && <span className="ml-1 font-normal">(Funkcionáři)</span>}
      </div>
    </div>
  );
}

function ClubEventCell({
  style,
  className,
  event,
  continuesPrior,
  continuesAfter,
}: EventCellProps & { event: Extract<CalendarEvent, { kind: 'event' }> }) {
  const auth = useAuth();
  const { instance } = event;
  const canEdit = auth.canManageEvent(instance);
  const isDragging = useAtomValue(isDraggingAtom);

  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback(
    (v: DragSubject) => (v?.event?.instance.id === instance.id ? v : null),
    [instance.id],
  );
  const currentDragSubject = useAtomValue(selectAtom(dragSubjectAtom, getCurrentEvent));

  const onTouchOrMouse = React.useCallback(
    (e: React.TouchEvent | React.MouseEvent) => {
      if (!canEdit || (e as React.MouseEvent).button || ('touches' in e && e.touches.length !== 1)) {
        return;
      }
      const resizeDirection = (e.target as HTMLElement).dataset.resize;
      if (resizeDirection) {
        setDragSubject({
          action: 'resize',
          event,
          direction: resizeDirection as DragDirection,
        });
      } else {
        setDragSubject({ action: 'move', event });
      }
    },
    [setDragSubject, event, canEdit],
  );

  return (
    <Popover modal>
      <PopoverTrigger asChild>
        <div
          style={style}
          onMouseDown={onTouchOrMouse}
          onTouchStart={onTouchOrMouse}
          className={cn(className, {
            'rbc-event group transition-opacity': true,
            'relative w-full h-full': canEdit,
            'rounded-l-none': continuesPrior,
            'rounded-r-none': continuesAfter,
            'cursor-grab': canEdit,
            'rbc-nondraggable': !canEdit,
            'rbc-drag-preview': event.__isPreview,
            'rbc-dragged-event': isDragging && currentDragSubject && !event.__isPreview,
            'pl-3': event.instance.targetCohortsList && event.instance.targetCohortsList.length > 0,
            relative: true,
          })}
        >
          <EventConflictsBadge
            eventId={event.instance.id}
            className="absolute right-1 top-1 text-accent-11 drop-shadow-sm"
          />
          {!!event.instance.targetCohortsList?.length && (
            <div className="absolute rounded-l-lg overflow-hidden border-r border-neutral-6 shadow-xs inset-y-0 left-0 flex flex-col">
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

          {!continuesPrior && canEdit && (
            <div
              className="absolute left-0 opacity-0 group-hover:opacity-100 cursor-w-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="LEFT"
            />
          )}

          <div
            className={`rbc-event-content${instance.isCancelled ? ' line-through' : ''}`}
          >
            {formatEventName(event.instance)}
          </div>

          {!continuesAfter && canEdit && (
            <div
              className="absolute right-0 opacity-0 group-hover:opacity-100 cursor-e-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="RIGHT"
            />
          )}
        </div>
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary event={instance} />
      </PopoverContent>
    </Popover>
  );
}

export default EventCell;
