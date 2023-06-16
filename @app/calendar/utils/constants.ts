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
  allDay?: boolean;
  title: React.ReactNode;
  start: Date;
  end: Date;
  resourceId: number;
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
  sourceResource?: number;
}

export interface Resource {
  resourceId: number;
  resourceTitle: React.ReactNode;
}

export interface DateRange {
  start: Date;
  end: Date;
}

export interface Bounds {
  x: number;
  y: number;
  top: number;
  bottom: number;
  left: number;
  right: number;
}

export interface Box {
  x: number;
  y: number;
  clientX: number;
  clientY: number;
}

export interface SlotInfo {
  start: Date | number; // TODO: wtf?
  end: Date | number;
  slots: (Date | number)[];
  action: 'select' | 'click';
  /** For "TimeGrid" views */
  resourceId?: number | string;
  /** For "select" action */
  bounds?: Bounds;
  /** For "click" actions */
  box?: Box;
}

export type ViewProps = {
  date: Date;
  events: Event[];
  backgroundEvents: Event[];
  resources: Resource[];
  selected?: Event;
  length?: number;
  min?: Date;
  max?: Date;

  onNavigate: (action: Navigate, newDate: Date) => void;
  onSelectEvent: (event: Event) => void
  onSelectSlot: (slotInfo: SlotInfo) => void;
  onDrillDown: (date: Date, view: View) => void;
};

export interface ViewClass extends React.FunctionComponent<ViewProps> {
  name: string;
  range: (date: Date, length?: number) => Date[];
  navigate(date: Date, action: Navigate, length?: number): Date;
  title(date: Date, length?: number): string;
}

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export interface EventInteractionArgs {
  event: Event;
  start: Date;
  end: Date;
  isAllDay: boolean;
}
