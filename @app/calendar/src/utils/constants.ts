export enum Navigate {
  PREVIOUS = 'PREV',
  NEXT = 'NEXT',
  TODAY = 'TODAY',
  DATE = 'DATE',
}

export enum View {
  MONTH = 'month',
  WEEK = 'week',
  WORK_WEEK = 'work_week',
  DAY = 'day',
  AGENDA = 'agenda',
}

export interface Event {
  allDay?: boolean | undefined;
  title: React.ReactNode;
  start: Date;
  end: Date;
  resourceId: number;
}

export interface Resource {
  resourceId: number;
  resourceTitle: React.ReactNode;
}

export interface DateRange {
  start: Date;
  end: Date;
}

export interface SlotInfo {
  start: Date;
  end: Date;
  slots: Date[];
  action: 'select' | 'click' | 'doubleClick';
  /** For "TimeGrid" views */
  resourceId?: number | string | undefined;
  /** For "select" action */
  bounds?:
    | {
        x: number;
        y: number;
        top: number;
        bottom: number;
        left: number;
        right: number;
      }
    | undefined;
  /** For "click" or "doubleClick" actions */
  box?:
    | {
        x: number;
        y: number;
        clientX: number;
        clientY: number;
      }
    | undefined;
}

export type DayLayoutFunction = (_: {
  events: Event[];
  minimumStartDifference: number;
  slotMetrics: any;
}) => Array<{ event: Event; style: React.CSSProperties }>;

export type DayLayoutAlgorithm = 'overlap' | 'no-overlap';

export type ViewProps = Omit<CalendarProps, 'className' | 'style' | 'view'> & {
  date: Date; // date has always a value, in contrast to optional date in CalendarProps

  getDrilldownView: any; // = this.getDrilldownView
  onNavigate: any; // = this.handleNavigate
  onDrillDown: any; // = this.handleDrillDown
  onSelectEvent: any; // = this.handleSelectEvent
  onDoubleClickEvent: any; // = this.handleDoubleClickEvent
  onSelectSlot: any; // = this.handleSelectSlot
};

export interface CalendarProps {
  date?: Date | undefined;
  view?: View | undefined;
  events?: Event[] | undefined;
  backgroundEvents?: Event[] | undefined;
  handleDragStart?: ((event: Event) => void) | undefined;
  onNavigate?: ((newDate: Date, view: View, action: Navigate) => void) | undefined;
  onView?: ((view: View) => void) | undefined;
  onDrillDown?: ((date: Date, view: View) => void) | undefined;
  onSelectSlot?: ((slotInfo: SlotInfo) => void) | undefined;
  onDoubleClickEvent?:
    | ((event: Event, e: React.SyntheticEvent<HTMLElement>) => void)
    | undefined;
  onSelectEvent?:
    | ((event: Event, e: React.SyntheticEvent<HTMLElement>) => void)
    | undefined;
  onKeyPressEvent?:
    | ((event: Event, e: React.SyntheticEvent<HTMLElement>) => void)
    | undefined;
  onSelecting?: (range: { start: Date; end: Date }) => boolean | undefined;
  onRangeChange?: (
    range: Date[] | { start: Date; end: Date },
    view?: View,
  ) => void | undefined;
  selected?: any;
  doShowMoreDrillDown?: boolean | undefined;
  drilldownView?: View | null | undefined;
  getDrilldownView?:
    | ((targetDate: Date, currentViewName: View) => void)
    | null
    | undefined;
  length?: number | undefined;
  popup?: boolean | undefined;
  step?: number | undefined;
  timeslots?: number | undefined;
  showMultiDayTimes?: boolean | undefined;
  min?: Date | undefined;
  max?: Date | undefined;
  scrollToTime?: Date | undefined;
  dayLayoutAlgorithm?: DayLayoutAlgorithm | DayLayoutFunction | undefined;
  resources?: Resource[] | undefined;
  defaultView?: View | undefined;
  defaultDate?: Date | undefined;
  className?: string | undefined;
  style?: React.CSSProperties | undefined;
  onShowMore?: ((events: Event[], date: Date) => void) | undefined;
}

export interface ViewStatic {
  range: (date: Date) => Date[];
  navigate(date: Date, action: Navigate, props: any): Date;
  title(date: Date): string;
}

export interface MoveOptions {
  action: Navigate;
  date: Date;
  today: Date;
}

export interface TimeGridProps {
  eventOffset: number;
  events?: Event[];
  backgroundEvents?: Event[];
  resources?: Resource[];
  step?: number;
  timeslots?: number;
  range?: any[];
  min?: Date;
  max?: Date;
  scrollToTime?: Date;
  showMultiDayTimes?: boolean;
  width?: number;
  selected?: object;
  onNavigate?: (action: Navigate) => void;
  onSelectSlot?: (slotInfo: SlotInfo) => void;
  onSelectEnd?: (...args: any[]) => any;
  onSelectStart?: (...args: any[]) => any;
  onSelectEvent?: (event: Event, e: React.SyntheticEvent<HTMLElement>) => void;
  onDoubleClickEvent?: (event: Event, e: React.SyntheticEvent<HTMLElement>) => void;
  onKeyPressEvent?: (...args: any[]) => any;
  onDrillDown?: (date: Date, view: View) => void;
  getDrilldownView?: (targetDate: Date, currentViewName: View) => void;
  dayLayoutAlgorithm?: any;
}

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export interface EventInteractionArgs {
  event: Event;
  start: Date;
  end: Date;
  isAllDay: boolean;
}

export interface OnDragStartArgs {
  event: Event;
  action: DragAction;
  direction: DragDirection;
}

export interface DragFromOutsideItemArgs {
  start: Date;
  end: Date;
  allDay: boolean;
}

export interface withDragAndDropProps {
  onEventDrop?: (args: EventInteractionArgs) => void;
  onEventResize?: (args: EventInteractionArgs) => void;
  onDragStart?: (args: OnDragStartArgs) => void;
  onDragOver?: (event: React.DragEvent) => void;
  onDropFromOutside?: (args: DragFromOutsideItemArgs) => void;
  dragFromOutsideItem?: () => keyof Event | ((event: Event) => Date);
  resizable?: boolean;
  step?: number;
}

interface DragAndDropCalendarProps extends CalendarProps, withDragAndDropProps {}

declare function withDragAndDrop(
  calendar: React.ComponentType<CalendarProps>,
): React.ComponentType<DragAndDropCalendarProps>;

export default withDragAndDrop;
