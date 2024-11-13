import type { EventInstanceWithEventFragment } from "@/graphql/Event";

export enum Navigate {
  PREVIOUS = 'PREV',
  NEXT = 'NEXT',
  TODAY = 'TODAY',
  DATE = 'DATE',
}

export interface CalendarEvent extends EventInstanceWithEventFragment {
  start: Date;
  end: Date;
  resourceIds: string[];
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
  sourceResource?: Resource;
}

export interface Resource {
  resourceId: string;
  resourceType: 'person' | 'location' | 'locationText' | '';
  resourceTitle: React.ReactNode;
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

export type SlotInfo = {
  start: Date;
  end: Date;
  slots: Date[];
  action: 'select' | 'click';
  /** For "TimeGrid" views */
  resource?: Resource;
  /** For "select" action */
  bounds?: {
    x: number;
    y: number;
    top: number;
    bottom: number;
    left: number;
    right: number;
  };
  /** For "click" actions */
  box?: {
    x: number;
    y: number;
    clientX?: number;
    clientY?: number;
  };
}

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export type InteractionInfo = {
  start: Date;
  end: Date;
  resource?: Resource;
};
