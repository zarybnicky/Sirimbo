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

export interface CalendarEvent {
  id: number;
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

export type ViewProps = {
  events: CalendarEvent[];
  range: Date[];
  backgroundEvents: CalendarEvent[];
  resources: Resource[];
  min?: Date;
  max?: Date;
  date: Date;
};

export interface ViewClass extends React.FunctionComponent<ViewProps> {
  range: (date: Date) => Date[];
  navigate(date: Date, action: Navigate): Date;
}
