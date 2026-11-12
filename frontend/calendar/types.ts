import type { CalendarDataQuery, EventWithTrainerFragment } from '@/graphql/Event';
import type { CompetitionEntry, CompetitionOfficialAssignment } from '@/ui/Competitions';

export type DateRange = {
  since: Date;
  until: Date;
};

export type CalendarTrainer = {
  id: string;
  name: string;
};

type CalendarItemBase = {
  start: Date;
  end: Date;
  resourceKeys: readonly string[];
};

export type CalendarClubEvent = CalendarItemBase & {
  kind: 'event';
  id: string;
  instance: EventWithTrainerFragment & Pick<
    NonNullable<CalendarDataQuery['eventsList']>[number], 'attendance'
  >;
  __isPreview?: boolean;
};

export type CalendarCompetitionEvent = CalendarItemBase & {
  kind: 'competition';
  id: string;
  title: string;
  event: NonNullable<CalendarDataQuery['federationEvents']>['nodes'][number];
  date: string;
  entries: readonly CompetitionEntry[];
  collapsedEntries: readonly CompetitionEntry[];
  officials?: readonly CompetitionOfficialAssignment[];
  collapsedOfficials: readonly CompetitionOfficialAssignment[];
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
  CalendarClubEvent | CalendarCompetitionEvent | CalendarBirthdayEvent;

export type ResourceRef =
  | { type: 'person' | 'location' | 'locationText'; id: string }
  | { type: 'competition' | 'unassigned' };

export type Resource = ResourceRef & { resourceTitle: string };

export function resourceKey(resource?: ResourceRef): string {
  if (!resource) return '__nothing__';
  return 'id' in resource ? `${resource.type}:${resource.id}` : resource.type;
}

export type ViewProps = {
  range: DateRange;
  events: readonly CalendarEvent[];
  backgroundEvents: readonly CalendarEvent[];
  resources: readonly Resource[];
  primary?: 'resource' | 'day';
  showAllResources?: boolean;
};

export type DragAction = 'resize' | 'move';

export type DragDirection = 'UP' | 'DOWN' | 'LEFT' | 'RIGHT';

export type InteractionInfo = {
  start: Date;
  end: Date;
  resource?: Resource;
};
