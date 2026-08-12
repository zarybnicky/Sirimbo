import { ActivityTimelineDocument } from '@/graphql/ActivityTimeline';
import type { EventType } from '@/graphql';
import { EventRangeDocument, type EventRangeQuery, type EventRangeQueryVariables } from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useClient, useQuery } from 'urql';
import type { CalendarCompetitionEvent, CalendarEvent, CalendarInstanceEvent, DateRange, Resource } from './types';
import { CalendarView } from '@/calendar/CalendarViews';
import { eventTypes as allEventTypes } from '@/calendar/state';

export type CalendarGroupBy = 'none' | 'trainer' | 'room';
export type CalendarFilters = {
  participantIds: string[];
  trainerIds: string[];
  eventTypes: EventType[];
  onlyMine: boolean;
  myPersonIds: string[];
  parentId?: string;
};

const competitionResource: Resource = {
  resourceId: '__competition__',
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
  { onlyMine, trainerIds, participantIds, eventTypes, parentId }: CalendarFilters,
): EventRangeQueryVariables {
  return {
    start: startOf(range.since, 'day').toISOString(),
    end: add(startOf(range.until, 'day'), 1, 'day').toISOString(),
    trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
    participantIds: participantIds.length > 0 ? participantIds : undefined,
    type: eventTypes.length === 1 ? eventTypes[0] : undefined,
    onlyMine,
    parentId,
  };
}

function mapEventsToCalendar(
  list: EventRangeQuery['list'] | undefined,
  groupBy: CalendarGroupBy,
  eventTypes: EventType[],
): { events: CalendarInstanceEvent[]; resources: Resource[] } {
  const events: CalendarInstanceEvent[] = [];
  const resourceMap = new Map<string, Resource>();
  const visibleTypes = new Set(eventTypes);
  const filterTypes = eventTypes.length !== allEventTypes.length;

  const assignResource = (
    resourceIds: string[],
    resourceId: string,
    resourceTitle: string,
  ) => {
    resourceIds.push(resourceId);
    resourceMap.set(resourceId, { resourceId, resourceTitle });
  };

  for (const instance of list ?? []) {
    if (filterTypes && (!instance.type || !visibleTypes.has(instance.type))) continue;
    const start = new Date(instance.since);
    const end = new Date(instance.until);
    const resourceIds: string[] = [];

    if (groupBy === 'trainer') {
      for (const { person, personId } of instance.trainersList) {
        assignResource(resourceIds, `person:${personId}`, person?.name || '');
      }
    } else if (groupBy === 'room') {
      const { location, locationText } = instance;
      if (location?.id) {
        assignResource(resourceIds, `location:${location.id}`, location.name);
      } else if (locationText) {
        assignResource(resourceIds, `locationText:${locationText}`, locationText);
      }
    }
    if (groupBy !== 'none' && resourceIds.length === 0) {
      assignResource(resourceIds, '', '-');
    }
    events.push({ kind: 'event', id: instance.id, instance, resourceIds, start, end });
  }

  return {
    events,
    resources: [...resourceMap.values()].toSorted((a, b) =>
      a.resourceTitle.localeCompare(b.resourceTitle),
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

  const [{ data, fetching }, refresh] = useQuery({
    query: EventRangeDocument,
    variables,
    requestPolicy: 'cache-and-network',
  });
  const effectiveGroupBy = filters.onlyMine ? 'none' : groupBy;
  const showActivities =
    !filters.parentId && filters.eventTypes.length === allEventTypes.length;
  const [{ data: activityData, fetching: activityFetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: variables.start,
      until: variables.end,
      personIds: filters.participantIds.length > 0
          ? filters.participantIds
          : filters.onlyMine
              ? filters.myPersonIds
              : undefined,
      kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT', 'BIRTHDAY'],
    },
    requestPolicy: 'cache-and-network',
    pause: !showActivities,
  });

  React.useEffect(() => {
    if (!view.nav) return;
    const prev = prepareVariables(view.range(view.nav(date, -1)), filters);
    const next = prepareVariables(view.range(view.nav(date, 1)), filters);
    const timeout = setTimeout(() => {
      void client
        .query(EventRangeDocument, prev, { requestPolicy: 'cache-first' })
        .toPromise();
      void client
        .query(EventRangeDocument, next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);
    return () => clearTimeout(timeout);
  }, [client, date, filters, view]);

  const { events, resources } = React.useMemo(() => {
    const schedule = mapEventsToCalendar(data?.list, effectiveGroupBy, filters.eventTypes);
    const competitionsByEvent = new Map<string, CalendarCompetitionEvent>();
    const seen = new Set<string>();
    const birthdays: CalendarEvent[] = [];

    for (const item of showActivities ? (activityData?.activityTimelineList ?? []) : []) {
      if (
        item.__typename === 'ActivityCompetitionBrief' ||
        item.__typename === 'ActivityCompetitionResult'
      ) {
        const date = item.competitionDate;
        const competitorKey = `${item.competitionId}:${item.competitorId}`;
        if (!date || seen.has(competitorKey)) continue;
        seen.add(competitorKey);

        const eventKey = `${date}:${item.competitionEventId ?? ''}`;
        const competition = competitionsByEvent.get(eventKey) ?? {
          kind: 'competition',
          id: `competition:${date}:${item.competitionEventId ?? 'unknown'}`,
          title: item.competitionEventName!,
          eventLocation: item.competitionEventLocation,
          ...calendarDay(date),
          resourceIds:
            effectiveGroupBy === 'none' ? [] : [competitionResource.resourceId],
          items: [],
        };
        competition.items.push(item);
        competitionsByEvent.set(eventKey, competition);
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
          resourceIds: [],
        });
      }
    }

    const competitions = [...competitionsByEvent.values()];
    for (const competition of competitions) {
      competition.items.sort(
        (a, b) =>
          a.__typename.localeCompare(b.__typename) ||
          (a.competitorName ?? '').localeCompare(b.competitorName ?? '') ||
          (a.category?.name ?? '').localeCompare(b.category?.name ?? ''),
      );
    }

    if (effectiveGroupBy !== 'none' && competitions.length > 0) {
      schedule.resources.push(competitionResource);
    }
    return {
      events: [...schedule.events, ...competitions, ...birthdays],
      resources: schedule.resources,
    };
  }, [
    activityData?.activityTimelineList,
    data?.list,
    effectiveGroupBy,
    filters.eventTypes,
    showActivities,
  ]);

  return {
    range,
    events,
    resources,
    fetching: fetching || (showActivities && activityFetching),
    refresh,
  };
}
