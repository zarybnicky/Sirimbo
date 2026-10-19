import type { EventWithTrainerFragment } from '@/graphql/Event';
import type { CompetitionEntry } from '@/ui/Competitions';

export type DateRange = {
  since: Date;
  until: Date;
};

type CalendarItemBase = {
  start: Date;
  end: Date;
  resourceKeys: readonly string[];
  isDraggable?: boolean;
  isResizable?: boolean;
  __isPreview?: boolean;
};

export type CalendarInstanceEvent = CalendarItemBase & {
  kind: 'event';
  id: string;
  instance: EventWithTrainerFragment;
};

export type CalendarCompetitionEvent = CalendarItemBase & {
  kind: 'competition';
  id: string;
  title: string;
  items: readonly CompetitionEntry[];
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

export type ResourceRef =
  | { type: 'person' | 'location' | 'locationText'; id: string }
  | { type: 'competition' | 'unassigned' };

export type Resource = ResourceRef & { resourceTitle: string };

export function resourceKey(resource?: ResourceRef): string {
  if (!resource) return '__nothing__';
  return 'id' in resource ? `${resource.type}:${resource.id}` : resource.type;
}

export type ViewProps = {
  parentId?: string;
  range: DateRange;
  events: readonly CalendarEvent[];
  backgroundEvents: readonly CalendarEvent[];
  resources: readonly Resource[];
  primary?: 'resource' | 'day';
  showAllResources?: boolean;
};

export type SlotInfo = {
  start: Date;
  end: Date;
  action: 'select' | 'click' | 'day';
  /** For "TimeGrid" views */
  resource?: Resource;
};

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export type InteractionInfo = {
  start: Date;
  end: Date;
  resource?: Resource;
};
