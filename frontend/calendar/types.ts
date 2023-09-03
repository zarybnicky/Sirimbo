import { EventInstanceWithRegistrationsFragment } from "@app/graphql/Event";

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

export interface CalendarEvent extends EventInstanceWithRegistrationsFragment {
  allDay?: boolean;
  title: React.ReactNode;
  start: Date;
  end: Date;
  resourceIds: number[];
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
  sourceResource?: number;
}

export interface Resource {
  resourceId: number;
  resourceTitle: React.ReactNode;
}

type ViewProps = {
  events: CalendarEvent[];
  range: Date[];
  backgroundEvents: CalendarEvent[];
  resources: Resource[];
  min?: Date;
  max?: Date;
  date: Date;
};

export type ViewClass =React.FunctionComponent<ViewProps>;
