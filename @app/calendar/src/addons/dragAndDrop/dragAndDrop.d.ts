import { CalendarProps, Event, stringOrDate } from '../../index';
import * as React from 'react';

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export interface EventInteractionArgs<TEvent> {
  event: TEvent;
  start: stringOrDate;
  end: stringOrDate;
  isAllDay: boolean;
}

export interface OnDragStartArgs<TEvent> {
  event: TEvent;
  action: DragAction;
  direction: DragDirection;
}

export interface DragFromOutsideItemArgs {
  start: stringOrDate;
  end: stringOrDate;
  allDay: boolean;
}

export interface withDragAndDropProps<TEvent extends object = Event> {
  onEventDrop?: ((args: EventInteractionArgs<TEvent>) => void) | undefined;
  onEventResize?: ((args: EventInteractionArgs<TEvent>) => void) | undefined;
  onDragStart?: ((args: OnDragStartArgs<TEvent>) => void) | undefined;
  onDragOver?: ((event: React.DragEvent) => void) | undefined;
  onDropFromOutside?: ((args: DragFromOutsideItemArgs) => void) | undefined;
  dragFromOutsideItem?: (() => keyof TEvent | ((event: TEvent) => Date)) | undefined;
  selectable?: true | false | 'ignoreEvents' | undefined;
  resizable?: boolean | undefined;
  step?: number | undefined;
}

interface DragAndDropCalendarProps<TEvent extends object = Event, TResource extends object = object>
  extends CalendarProps<TEvent, TResource>, withDragAndDropProps<TEvent, TResource> {}

declare function withDragAndDrop<TEvent extends object = Event, TResource extends object = object>(
    calendar: React.ComponentType<CalendarProps<TEvent, TResource>>
): React.ComponentType<DragAndDropCalendarProps<TEvent, TResource>>;

export default withDragAndDrop;

// Turn off automatic exports
export {};
