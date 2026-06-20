import type { EventFragment, EventInstanceWithTrainerFragment } from '@/graphql/Event';
import type { CompetitionEntry } from '@/ui/Competitions';

export type DateRange = {
  since: Date;
  until: Date;
};

type CalendarItemBase = {
  start: Date;
  end: Date;
  resourceIds: readonly string[];
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
  sourceResource?: Resource;
};

export type CalendarInstanceEvent = CalendarItemBase & {
  kind: 'event';
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
};

export type CalendarCompetitionEvent = CalendarItemBase & {
  kind: 'competition';
  id: string;
  title: string;
  eventLocation: string | null;
  items: CompetitionEntry[];
};

export type CalendarBirthdayEvent = CalendarItemBase & {
  kind: 'birthday';
  id: string;
  person: {
    id: string;
    name: string;
    firstName: string;
    lastName: string;
  };
  date: string;
};

export type CalendarEvent =
  | CalendarInstanceEvent
  | CalendarCompetitionEvent
  | CalendarBirthdayEvent;

export interface Resource {
  resourceId: string;
  resourceTitle: string;
}

export type ViewProps = {
  range: DateRange;
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
