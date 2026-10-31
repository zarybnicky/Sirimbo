import { groupCompetitionEvents, type CompetitionEntry } from '@/ui/Competitions';
import { ActivityTimelineDocument } from '@/graphql/ActivityTimeline';
import type { EventCondition, EventType } from '@/graphql';
import {
  CalendarFacetsDocument,
  EventListDocument,
  type EventListQuery,
  type EventListQueryVariables,
} from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useAtomValue } from 'jotai';
import { useClient, useQuery } from 'urql';
import {
  resourceKey,
  type CalendarCompetitionEvent,
  type CalendarEvent,
  type CalendarInstanceEvent,
  type CalendarTrainer,
  type DateRange,
  type Resource,
} from './types';
import type { CalendarView } from '@/calendar/CalendarViews';
import { eventTypes as allEventTypes, groupByAtom } from '@/calendar/state';
import { compareText } from '@/lib/compare';
import { useAuth } from '@/lib/auth';
import { formatEventType } from '@/ui/format';

export type CalendarGroupBy = 'none' | 'trainer' | 'room';
export type CalendarFilters = {
  participants: string[];
  trainers: string[];
  types: EventType[];
  my: boolean;
  free: boolean;
  location: string[];
  columns: 'all' | null;
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

function prepareVariables(range: DateRange, condition: EventCondition) {
  return {
    condition: {
      ...condition,
      overlaps: {
        start: startOf(range.since, 'day').toISOString(),
        end: add(startOf(range.until, 'day'), 1, 'day').toISOString(),
      },
    },
    orderBy: ['SINCE_ASC'],
  } satisfies EventListQueryVariables;
}

function mapEventsToCalendar(
  list: EventListQuery['eventsList'] | undefined,
  groupBy: CalendarGroupBy,
): { events: CalendarInstanceEvent[]; resources: Map<string, Resource> } {
  const events: CalendarInstanceEvent[] = [];
  const resourceMap = new Map<string, Resource>();

  const assignResource = (resourceKeys: string[], resource: Resource) => {
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
        assignResource(resourceKeys, {
          type: 'person',
          id: personId,
          resourceTitle: person?.name || '',
        });
      }
    } else if (groupBy === 'room') {
      const { location, locationText } = instance;
      if (location?.id) {
        assignResource(resourceKeys, {
          type: 'location',
          id: location.id,
          resourceTitle: location.name,
        });
      } else if (locationText) {
        assignResource(resourceKeys, {
          type: 'locationText',
          id: locationText,
          resourceTitle: locationText,
        });
      }
    }
    if (groupBy !== 'none' && resourceKeys.length === 0) {
      assignResource(resourceKeys, { type: 'unassigned', resourceTitle: '-' });
    }
    events.push({ kind: 'event', id: instance.id, instance, resourceKeys, start, end });
  }

  return {
    events,
    resources: resourceMap,
  };
}

export function useCalendarData({
  view,
  date,
  filters,
  parentId,
  availableTrainers,
}: {
  view: CalendarView;
  date: Date;
  filters: CalendarFilters;
  parentId?: string;
  availableTrainers?: readonly CalendarTrainer[];
}) {
  const auth = useAuth();
  const groupBy = useAtomValue(groupByAtom);
  const onlyMine = auth.isLoggedIn && filters.my;
  const showAllResources = filters.columns === 'all';
  const client = useClient();
  const range = React.useMemo(() => view.range(date), [view, date]);
  const condition = React.useMemo(() => {
    const trainerIds = availableTrainers
      ? filters.trainers.filter((id) => availableTrainers.some((t) => t.id === id))
      : filters.trainers;
    return {
      trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
      participantIds:
        auth.isLoggedIn && filters.participants.length > 0
          ? filters.participants
          : undefined,
      locationIds: filters.location.length > 0 ? filters.location : undefined,
      types: filters.free
        ? filters.types.filter((type) => type !== 'GROUP')
        : filters.types,
      canRegister: filters.free ? true : undefined,
      onlyMine,
      relevantOnly: !parentId && filters.location.length === 0,
      parentId,
    } satisfies EventCondition;
  }, [
    availableTrainers,
    auth.isLoggedIn,
    onlyMine,
    filters.free,
    filters.trainers,
    filters.participants,
    filters.location,
    filters.types,
    parentId,
  ]);
  const variables = prepareVariables(range, condition);
  const { overlaps } = variables.condition;

  const [{ data, fetching }] = useQuery({ query: EventListDocument, variables });
  const [facetResult] = useQuery({
    query: CalendarFacetsDocument,
    variables: {
      ...condition,
      start: overlaps.start,
      end: overlaps.end,
      parentId,
      withParticipantOptions: auth.isLoggedIn,
    },
  });
  const facetData = facetResult.data;
  const trainers = React.useMemo(
    () =>
      availableTrainers?.toSorted((a, b) => compareText(a.name, b.name)) ??
      (facetData?.tenant?.tenantTrainersList ?? [])
        .flatMap(({ person }) => (person ? [person] : []))
        .toSorted(
          (a, b) =>
            compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
        ),
    [availableTrainers, facetData?.tenant?.tenantTrainersList],
  );
  const filterOptions = React.useMemo(() => {
    const counts: Record<string, Record<string, number>> = {};
    for (const { facet, value, count } of facetData?.eventFacetsList ?? []) {
      if (facet && value) (counts[facet] ??= {})[value] = count ?? 0;
    }
    const count = (facet: string, x: string) =>
      facetData ? (counts[facet]?.[x] ?? 0) : undefined;
    const participantIds = condition.participantIds ?? [];
    const trainerIds = condition.trainerIds ?? [];
    return {
      types: allEventTypes.map((type) => ({
        value: type,
        label: formatEventType(type),
        count: count('type', type),
      })),
      trainers: trainers
        .filter(
          ({ id }) =>
            showAllResources ||
            !facetData ||
            !!counts.trainer?.[id] ||
            trainerIds.includes(id),
        )
        .map(({ id, name }) => ({ value: id, label: name, count: count('trainer', id) })),
      participants: auth.isLoggedIn
        ? (facetData?.people?.nodes ?? [])
            .flatMap((person) => (person ? [person] : []))
            .filter(({ id }) => !!counts.participant?.[id] || participantIds.includes(id))
            .toSorted(
              (a, b) =>
                Number(participantIds.includes(b.id)) -
                Number(participantIds.includes(a.id)),
            )
            .map(({ id, name }) => ({
              value: id,
              label: name,
              count: count('participant', id),
            }))
        : [],
      locations: (facetData?.tenant?.locationsList ?? [])
        .filter(
          ({ id, showInLists }) =>
            auth.isAdmin || showInLists || filters.location.includes(id),
        )
        .map(({ id, name }) => ({
          value: id,
          label: name,
          count: count('location', id),
        })),
    };
  }, [
    facetData,
    trainers,
    condition.participantIds,
    condition.trainerIds,
    filters.location,
    showAllResources,
    auth.isLoggedIn,
    auth.isAdmin,
  ]);

  const effectiveGroupBy = onlyMine ? 'none' : groupBy;
  const showActivities =
    !filters.free &&
    !parentId &&
    !condition.locationIds &&
    !condition.trainerIds &&
    allEventTypes.every((type) => filters.types.includes(type));
  const [{ data: activityData, fetching: activityFetching }] = useQuery({
    query: ActivityTimelineDocument,
    variables: {
      since: overlaps.start,
      until: overlaps.end,
      personIds: onlyMine
        ? auth.personIds.filter(
            (id) => !condition.participantIds || condition.participantIds.includes(id),
          )
        : condition.participantIds,
      kinds: ['COMPETITION_BRIEF', 'COMPETITION_RESULT', 'BIRTHDAY'],
    },
    pause: !showActivities,
  });

  React.useEffect(() => {
    if (!view.nav) return;
    const prev = prepareVariables(view.range(view.nav(date, -1)), condition);
    const next = prepareVariables(view.range(view.nav(date, 1)), condition);
    const timeout = setTimeout(() => {
      void client
        .query(EventListDocument, prev, { requestPolicy: 'cache-first' })
        .toPromise();
      void client
        .query(EventListDocument, next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);
    return () => clearTimeout(timeout);
  }, [client, date, condition, view]);

  const { events: calendarEvents, resources } = React.useMemo(() => {
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

    if (effectiveGroupBy === 'trainer') {
      for (const trainer of trainers) {
        const resource: Resource = {
          type: 'person',
          id: trainer.id,
          resourceTitle: trainer.name,
        };
        schedule.resources.set(resourceKey(resource), resource);
      }
    }
    if (effectiveGroupBy !== 'none' && competitions.length > 0) {
      schedule.resources.set(resourceKey(competitionResource), competitionResource);
    }
    return {
      events: [...schedule.events, ...competitions, ...birthdays],
      resources: [...schedule.resources.values()]
        .filter(
          (resource) =>
            effectiveGroupBy !== 'trainer' ||
            !condition.trainerIds ||
            (resource.type === 'person' && condition.trainerIds.includes(resource.id)),
        )
        .toSorted((a, b) => compareText(a.resourceTitle, b.resourceTitle)),
    };
  }, [
    activityData?.activityTimelineList,
    data?.eventsList,
    effectiveGroupBy,
    showActivities,
    trainers,
    condition.trainerIds,
  ]);

  return {
    range,
    events: data?.eventsList ?? [],
    calendarEvents,
    resources,
    fetching: fetching || facetResult.fetching || (showActivities && activityFetching),
    filterOptions,
  };
}
