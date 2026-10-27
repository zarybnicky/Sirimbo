import { groupCompetitionEvents, type CompetitionEntry } from '@/ui/Competitions';
import { ActivityTimelineDocument } from '@/graphql/ActivityTimeline';
import type { EventType } from '@/graphql';
import {
  CalendarFacetsDocument,
  EventListDocument,
  type EventListQuery,
  type EventListQueryVariables,
} from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useClient, useQuery, type OperationContext } from 'urql';
import {
  resourceKey,
  type CalendarCompetitionEvent,
  type CalendarEvent,
  type CalendarInstanceEvent,
  type DateRange,
  type Resource,
} from './types';
import { CalendarView } from '@/calendar/CalendarViews';
import { eventTypes as allEventTypes } from '@/calendar/state';
import { compareText } from '@/lib/compare';

export type CalendarGroupBy = 'none' | 'trainer' | 'room';
export type CalendarFilters = {
  participantIds: string[];
  trainerIds: string[];
  eventTypes: EventType[];
  onlyMine: boolean;
  myPersonIds: string[];
  parentId?: string;
  locationId?: string | null;
};

const competitionResource: Resource = {
  type: 'competition',
  resourceTitle: 'Soutěže',
};

const calendarDay = (date: string) => ({
  start: new Date(`${date}T00:00:00`),
  end: new Date(`${date}T23:59:00`),
  isDraggable: false,
  isResizable: false,
});

function prepareVariables(
  range: DateRange,
  {
    onlyMine,
    trainerIds,
    participantIds,
    eventTypes,
    parentId,
    locationId,
  }: CalendarFilters,
) {
  return {
    condition: {
      overlaps: {
        start: startOf(range.since, 'day').toISOString(),
        end: add(startOf(range.until, 'day'), 1, 'day').toISOString(),
      },
      trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
      participantIds: participantIds.length > 0 ? participantIds : undefined,
      locationIds: locationId ? [locationId] : undefined,
      types: eventTypes,
      scope: {
        mode: onlyMine ? 'MINE' : parentId || locationId ? 'ALL' : 'RELEVANT',
        parentId,
      },
    },
    orderBy: ['SINCE_ASC'],
  } satisfies EventListQueryVariables;
}

function mapEventsToCalendar(
  list: EventListQuery['eventsList'] | undefined,
  groupBy: CalendarGroupBy,
): { events: CalendarInstanceEvent[]; resources: Resource[] } {
  const events: CalendarInstanceEvent[] = [];
  const resourceMap = new Map<string, Resource>();

  const assignResource = (
    resourceKeys: string[],
    resource: Resource,
  ) => {
    const key = resourceKey(resource);
    resourceKeys.push(key);
    resourceMap.set(key, resource);
  };

  for (const instance of list ?? []) {
    const start = new Date(instance.since);
    const end = new Date(instance.until);
    const resourceKeys: string[] = [];

    if (groupBy === 'trainer') {
      for (const { person, personId } of instance.eventTrainersList) {
        assignResource(resourceKeys, { type: 'person', id: personId, resourceTitle: person?.name || '' });
      }
    } else if (groupBy === 'room') {
      const { location, locationText } = instance;
      if (location?.id) {
        assignResource(resourceKeys, { type: 'location', id: location.id, resourceTitle: location.name });
      } else if (locationText) {
        assignResource(resourceKeys, { type: 'locationText', id: locationText, resourceTitle: locationText });
      }
    }
    if (groupBy !== 'none' && resourceKeys.length === 0) {
      assignResource(resourceKeys, { type: 'unassigned', resourceTitle: '-' });
    }
    events.push({ kind: 'event', id: instance.id, instance, resourceKeys, start, end });
  }

  return {
    events,
    resources: [...resourceMap.values()].toSorted((a, b) =>
      compareText(a.resourceTitle, b.resourceTitle),
    ),
  };
}

export function useCalendarData(
  view: CalendarView,
  date: Date,
  filters: CalendarFilters,
  groupBy: CalendarGroupBy,
) {
  const client = useClient();
  const range = React.useMemo(() => {
    return view.range(date);
  }, [view, date]);
  const variables = prepareVariables(range, filters);
  const { overlaps, scope, ...condition } = variables.condition;

  const [{ data, fetching }, refresh] = useQuery({ query: EventListDocument, variables });
  const [facetResult, refreshFacets] = useQuery({
    query: CalendarFacetsDocument,
    variables: {
      ...condition,
      start: overlaps.start,
      end: overlaps.end,
      scope: scope.mode,
      parentId: scope.parentId,
    },
  });
  const [facetData, setFacetData] = React.useState(facetResult.data);
  if (facetResult.data !== undefined && facetResult.data !== facetData) {
    setFacetData(facetResult.data);
  }
  const facets: Record<string, Record<string, number>> | undefined = facetData
    ? { type: {}, trainer: {}, participant: {}, location: {} }
    : undefined;
  for (const { facet, value, count } of facetData?.eventFacetsList ?? []) {
    (facets![facet!] ??= {})[value!] = count ?? 0;
  }

  const refreshCalendar = React.useCallback((context?: Partial<OperationContext>) => {
    refresh(context);
    refreshFacets(context);
  }, [refresh, refreshFacets]);

  const effectiveGroupBy = filters.onlyMine ? 'none' : groupBy;
  const showActivities =
    !filters.parentId &&
    !filters.locationId &&
    filters.eventTypes.length === allEventTypes.length;
  const [{ data: activityData, fetching: activityFetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: overlaps.start,
      until: overlaps.end,
      personIds:
        filters.participantIds.length > 0
          ? filters.participantIds
          : filters.onlyMine
            ? filters.myPersonIds
            : undefined,
      kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT', 'BIRTHDAY'],
    },
    pause: !showActivities,
  });

  React.useEffect(() => {
    if (!view.nav) return;
    const prev = prepareVariables(view.range(view.nav(date, -1)), filters);
    const next = prepareVariables(view.range(view.nav(date, 1)), filters);
    const timeout = setTimeout(() => {
      void client
        .query(EventListDocument, prev, { requestPolicy: 'cache-first' })
        .toPromise();
      void client
        .query(EventListDocument, next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);
    return () => clearTimeout(timeout);
  }, [client, date, filters, view]);

  const { events, resources } = React.useMemo(() => {
    const schedule = mapEventsToCalendar(data?.eventsList, effectiveGroupBy);
    const competitionItems: CompetitionEntry[] = [];
    const birthdays: CalendarEvent[] = [];

    for (const item of showActivities ? (activityData?.activityTimelineList ?? []) : []) {
      if (
        item.__typename === 'ActivityCompetitionBrief' ||
        item.__typename === 'ActivityCompetitionResult'
      ) {
        if (item.competitionDate) competitionItems.push(item);
      } else if (item.__typename === 'ActivityBirthday') {
        const date = item.activityDate!;
        birthdays.push({
          kind: 'birthday',
          id: item.id,
          person: {
            id: item.personId!,
            name: item.person?.name!,
            firstName: item.person?.firstName!,
            lastName: item.person?.lastName!,
          },
          date,
          ...calendarDay(date),
          resourceKeys: [],
        });
      }
    }

    const competitions: CalendarCompetitionEvent[] = groupCompetitionEvents(
      competitionItems,
    ).map(({ key, entries }) => ({
      kind: 'competition',
      id: `competition:${key}`,
      title: entries[0]!.competitionEventName ?? '',
      ...calendarDay(entries[0]!.competitionDate!),
      resourceKeys: effectiveGroupBy === 'none' ? [] : [resourceKey(competitionResource)],
      items: entries,
    }));

    if (effectiveGroupBy !== 'none' && competitions.length > 0) {
      schedule.resources.push(competitionResource);
    }
    return {
      events: [...schedule.events, ...competitions, ...birthdays],
      resources: schedule.resources,
    };
  }, [activityData?.activityTimelineList, data?.eventsList, effectiveGroupBy, showActivities]);

  return {
    range,
    events,
    instances: data?.eventsList ?? [],
    resources,
    fetching: fetching || (showActivities && activityFetching),
    facets,
    refresh: refreshCalendar,
  };
}
