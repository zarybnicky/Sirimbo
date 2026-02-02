import type { EventFragment, EventInstanceWithTrainerFragment } from '@/graphql/Event';

export interface CalendarEvent {
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
  start: Date;
  end: Date;
  resourceIds: readonly string[];
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
  sourceResource?: Resource;
}

export interface Resource {
  resourceId: string;
  resourceTitle: string;
}

export type ViewProps = {
  date: Date;
  range: Date[];
  events: readonly CalendarEvent[];
  backgroundEvents: readonly CalendarEvent[];
  resources: readonly Resource[];
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
};

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export type InteractionInfo = {
  start: Date;
  end: Date;
  resource?: Resource;
};
