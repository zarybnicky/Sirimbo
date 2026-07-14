import { ActivityTimelineDocument } from '@/graphql/ActivityTimeline';
import type { EventType } from '@/graphql';
import { type CompetitionEntry } from '@/ui/Competitions';
import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
  type EventInstanceRangeQueryVariables,
} from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useClient, useQuery } from 'urql';
import type { CalendarEvent, CalendarInstanceEvent, DateRange, Resource } from './types';
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

type CompetitionBucket = {
  date: string;
  title: string;
  eventLocation: string | null;
  items: CompetitionEntry[];
};

function prepareVariables(
  range: DateRange,
  { onlyMine, trainerIds, participantIds, eventTypes, parentId }: CalendarFilters,
): EventInstanceRangeQueryVariables {
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

function mapInstancesToCalendar(
  list: EventInstanceRangeQuery['list'],
  groupBy: CalendarGroupBy,
  additionalResources: readonly Resource[],
  eventTypes: EventType[],
): { events: CalendarInstanceEvent[]; resources: Resource[] } {
  const events: CalendarInstanceEvent[] = [];
  const resourceMap = new Map<string, Resource>();
  const visibleTypes = new Set(eventTypes);
  const filterTypes = eventTypes.length !== allEventTypes.length;

  const put = (resource: Resource) => resourceMap.set(resource.resourceId, resource);
  if (groupBy !== 'none') {
    for (const resource of additionalResources) put(resource);
  }

  for (const instance of list ?? []) {
    if (filterTypes && (!instance.type || !visibleTypes.has(instance.type))) continue;
    const start = new Date(instance.since);
    const end = new Date(instance.until);
    const resourceIds: string[] = [];

    if (groupBy === 'trainer') {
      for (const trainer of instance.trainersList ?? []) {
        const id = trainer.personId;
        if (!id) continue;
        const resourceId = `person:${id}`;
        resourceIds.push(resourceId);
        put({
          resourceId,
          resourceTitle: trainer.person?.name || '',
        });
      }
    } else if (groupBy === 'room') {
      if (instance.location?.id) {
        const resourceId = `location:${instance.location.id}`;
        resourceIds.push(resourceId);
        put({
          resourceId,
          resourceTitle: instance.location.name,
        });
      } else if (instance.locationText) {
        const resourceId = `locationText:${instance.locationText}`;
        resourceIds.push(resourceId);
        put({
          resourceId,
          resourceTitle: instance.locationText,
        });
      }
    }
    if (groupBy !== 'none' && resourceIds.length === 0) {
      resourceIds.push('');
      put({ resourceId: '', resourceTitle: '-' });
    }
    events.push({ kind: 'event', instance, resourceIds, start, end });
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
  additionalResources: readonly Resource[],
) {
  const client = useClient();

  const { range, variables: vars } = React.useMemo(() => {
    const range = view.range(date);
    const prev = view.nav ? view.range(view.nav(date, -1)) : range;
    const next = view.nav ? view.range(view.nav(date, 1)) : range;

    return {
      range,
      variables: {
        current: prepareVariables(range, filters),
        prev: prepareVariables(prev, filters),
        next: prepareVariables(next, filters),
      },
    };
  }, [view, date, filters]);

  const [{ data, fetching }, refresh] = useQuery({
    query: EventInstanceRangeDocument,
    variables: vars.current,
    requestPolicy: 'cache-and-network',
  });
  const effectiveGroupBy = filters.onlyMine ? 'none' : groupBy;
  const factRange = React.useMemo(
    () => ({
      since: startOf(range.since, 'day').toISOString(),
      until: add(startOf(range.until, 'day'), 1, 'day').toISOString(),
    }),
    [range.since, range.until],
  );
  const factPersonIds =
    filters.participantIds.length > 0
      ? filters.participantIds
      : filters.onlyMine
        ? filters.myPersonIds
        : undefined;
  const showActivities =
    !filters.parentId && filters.eventTypes.length === allEventTypes.length;
  const [{ data: activityData, fetching: activityFetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: factRange.since,
      until: factRange.until,
      personIds: factPersonIds,
      kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT', 'BIRTHDAY'],
    },
    requestPolicy: 'cache-and-network',
    pause: !showActivities,
  });

  React.useEffect(() => {
    if (!view.nav) return;

    const timeout = setTimeout(() => {
      client
        .query(EventInstanceRangeDocument, vars.prev, { requestPolicy: 'cache-first' })
        .toPromise();
      client
        .query(EventInstanceRangeDocument, vars.next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);

    return () => clearTimeout(timeout);
  }, [client, vars.prev, vars.next, view.nav]);

  const { events, resources } = React.useMemo(() => {
    const schedule = mapInstancesToCalendar(
      data?.list ?? null,
      effectiveGroupBy,
      additionalResources,
      filters.eventTypes,
    );
    const competitionsByEvent = new Map<string, CompetitionBucket>();
    const seen = new Set<string>();
    const birthdays: CalendarEvent[] = [];
    const addCompetition = (item: CompetitionEntry) => {
      if (!item.competitionDate || seen.has(`${item.competitionId}:${item.competitorId}`))
        return;
      seen.add(`${item.competitionId}:${item.competitorId}`);

      const eventKey = `${item.competitionDate}:${item.competitionEventId ?? ''}`;
      const competition = competitionsByEvent.get(eventKey) ?? {
        date: item.competitionDate,
        title: item.competitionEventName!,
        eventLocation: item.competitionEventLocation,
        items: [],
      };
      competition.items.push(item);
      competitionsByEvent.set(eventKey, competition);
    };

    for (const item of showActivities ? (activityData?.activityTimelineList ?? []) : []) {
      if (
        item.__typename === 'ActivityCompetitionBrief' ||
        item.__typename === 'ActivityCompetitionResult'
      ) {
        addCompetition(item);
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
          start: new Date(`${date}T00:00:00`),
          end: new Date(`${date}T23:59:00`),
          resourceIds: [],
          isDraggable: false,
          isResizable: false,
        });
      }
    }

    const competitions: CalendarEvent[] = [...competitionsByEvent.values()].map(
      ({ date, items, title, eventLocation }) => ({
        kind: 'competition',
        id: `competition:${date}:${items[0]?.competitionEventId ?? 'unknown'}`,
        title,
        eventLocation,
        start: new Date(`${date}T00:00:00`),
        end: new Date(`${date}T23:59:00`),
        resourceIds: effectiveGroupBy === 'none' ? [] : [competitionResource.resourceId],
        isDraggable: false,
        isResizable: false,
        items: items.toSorted(
          (a, b) =>
            a.__typename.localeCompare(b.__typename) ||
            (a.competitorName ?? '').localeCompare(b.competitorName ?? '') ||
            (a.category?.name ?? '').localeCompare(b.category?.name ?? ''),
        ),
      }),
    );

    if (effectiveGroupBy !== 'none' && competitions.length > 0) {
      schedule.resources.push(competitionResource);
    }
    return {
      events: [...schedule.events, ...competitions, ...birthdays],
      resources: schedule.resources,
    };
  }, [
    activityData?.activityTimelineList,
    additionalResources,
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
