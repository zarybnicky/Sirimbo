import {
  competitionEntries,
  groupCompetitionEvents,
  type CompetitionOfficialAssignment,
} from '@/ui/Competitions';
import { format } from 'date-fns';
import type { CompetitionParticipantCondition } from '@/graphql';
import { CalendarDataDocument, type CalendarDataQueryVariables } from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useAtomValue } from 'jotai';
import { useClient, useQuery } from 'urql';
import {
  resourceKey,
  type CalendarEvent,
  type CalendarCompetitionEvent,
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
export const calendarTypeOptions = [
  ...allEventTypes.map((value) => ({ value, label: formatEventType(value) })),
  { value: 'COMPETITION', label: 'Soutěže' },
  { value: 'BIRTHDAY', label: 'Narozeniny' },
] as const;
export const calendarTypes = calendarTypeOptions.map(({ value }) => value);

export type CalendarFilters = {
  participants: CompetitionParticipantCondition;
  trainers: string[];
  types: (typeof calendarTypes)[number][];
  relevantOnly: boolean;
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

function prepareVariables(
  range: DateRange,
  variables: Omit<CalendarDataQueryVariables, 'start' | 'end' | 'since' | 'until'>,
): CalendarDataQueryVariables {
  const start = startOf(range.since, 'day');
  const end = add(startOf(range.until, 'day'), 1, 'day');
  return {
    ...variables,
    start: start.toISOString(),
    end: end.toISOString(),
    since: format(start, 'yyyy-MM-dd'),
    until: format(end, 'yyyy-MM-dd'),
  };
}

export function useCalendarData({
  view,
  date,
  filters,
  parentId,
  availableTrainers,
  withFacets = true,
  withAttendance = false,
}: {
  view: CalendarView;
  date: Date;
  filters: CalendarFilters;
  parentId?: string;
  availableTrainers?: readonly CalendarTrainer[];
  withFacets?: boolean;
  withAttendance?: boolean;
}) {
  const auth = useAuth();
  const groupBy = useAtomValue(groupByAtom);
  const onlyMine = auth.isLoggedIn && !!filters.participants.onlyMine;
  const showAllResources = filters.columns === 'all';
  const client = useClient();
  const range = React.useMemo(() => view.range(date), [view, date]);
  const trainerIds = availableTrainers
    ? filters.trainers.filter((id) => availableTrainers.some((t) => t.id === id))
    : filters.trainers;

  const [{ data, fetching, error, operation }] = useQuery({
    query: CalendarDataDocument,
    variables: prepareVariables(range, {
      trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
      participant: { ...filters.participants, onlyMine },
      participantIds: filters.participants.personIds,
      membership: {
        state: 'CURRENT',
        inCohorts: filters.participants.cohortId
          ? [filters.participants.cohortId]
          : undefined,
      },
      locationIds: filters.location.length > 0 ? filters.location : undefined,
      types: allEventTypes.filter(
        (type) => filters.types.includes(type) && (!filters.free || type !== 'GROUP'),
      ),
      canRegister: filters.free ? true : undefined,
      onlyMine,
      relevantOnly: filters.relevantOnly,
      parentId,
      withFacets,
      withAttendance,
      withCompetitions: filters.types.includes('COMPETITION'),
      withBirthdays: filters.types.includes('BIRTHDAY'),
      withParticipantOptions: auth.isLoggedIn,
    }),
  });

  const effectiveGroupBy = onlyMine ? 'none' : groupBy;

  React.useEffect(() => {
    if (fetching || !operation || !view.nav) return;
    const prev = prepareVariables(view.range(view.nav(date, -1)), operation.variables);
    const next = prepareVariables(view.range(view.nav(date, 1)), operation.variables);
    const timeout = setTimeout(() => {
      void client
        .query(CalendarDataDocument, prev, { requestPolicy: 'cache-first' })
        .toPromise();
      void client
        .query(CalendarDataDocument, next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);
    return () => clearTimeout(timeout);
  }, [client, date, fetching, operation, view]);

  return React.useMemo(() => {
    const trainers =
      availableTrainers?.toSorted((a, b) => compareText(a.name, b.name)) ??
      (data?.tenant?.tenantTrainersList ?? [])
        .flatMap(({ person }) => (person ? [person] : []))
        .toSorted(
          (a, b) =>
            compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
        );
    const counts: Record<string, Record<string, number>> = {};
    for (const { facet, value, count } of data?.eventFacetsList ?? []) {
      if (facet && value) (counts[facet] ??= {})[value] = count ?? 0;
    }
    const option = <T extends string>(facet: string, value: T, label: string) => ({
      value,
      label,
      count: data ? (counts[facet]?.[value] ?? 0) : undefined,
    });
    const filterOptions = withFacets
      ? {
          types: calendarTypeOptions.map(({ value, label }) =>
            value === 'COMPETITION' || value === 'BIRTHDAY'
              ? { value, label }
              : option('type', value, label),
          ),
          trainers: trainers
            .filter(
              ({ id }) =>
                showAllResources ||
                !data ||
                !!counts.trainer?.[id] ||
                filters.trainers.includes(id),
            )
            .map(({ id, name }) => option('trainer', id, name)),
          participants: (data?.people?.nodes ?? [])
            .filter(
              ({ id }) =>
                !!counts.participant?.[id] ||
                filters.participants.personIds?.includes(id),
            )
            .toSorted(
              (a, b) =>
                Number(filters.participants.personIds?.includes(b.id) ?? false) -
                Number(filters.participants.personIds?.includes(a.id) ?? false),
            )
            .map(({ id, name }) => option('participant', id, name)),
          locations: (data?.tenant?.locationsList ?? [])
            .filter(
              ({ id, showInLists }) =>
                auth.isAdmin || showInLists || filters.location.includes(id),
            )
            .map(({ id, name }) => option('location', id, name)),
        }
      : { types: [], trainers: [], participants: [], locations: [] };

    const calendarEvents: CalendarEvent[] = [];
    const resources = new Map<string, Resource>();
    const addResource = (resource: Resource) => {
      const key = resourceKey(resource);
      resources.set(key, resource);
      return key;
    };

    for (const instance of data?.eventsList ?? []) {
      let eventResources: Resource[] = [];
      if (effectiveGroupBy === 'trainer') {
        eventResources = instance.eventTrainersList.map(({ person, personId }) => ({
          type: 'person',
          id: personId,
          resourceTitle: person?.name || '',
        }));
      } else if (effectiveGroupBy === 'room') {
        const { location, locationText } = instance;
        if (location?.id) {
          eventResources.push({
            type: 'location',
            id: location.id,
            resourceTitle: location.name,
          });
        } else if (locationText) {
          eventResources.push({
            type: 'locationText',
            id: locationText,
            resourceTitle: locationText,
          });
        }
      }
      if (effectiveGroupBy !== 'none' && eventResources.length === 0) {
        eventResources.push({ type: 'unassigned', resourceTitle: '-' });
      }
      calendarEvents.push({
        kind: 'event',
        id: instance.id,
        instance,
        start: new Date(instance.since),
        end: new Date(instance.until),
        resourceKeys: eventResources.map(addResource),
      });
    }

    const competitionItems = competitionEntries(data?.competitions?.nodes ?? []);
    const officials: CompetitionOfficialAssignment[] = [
      ...(data?.officialEvents?.nodes ?? []).flatMap((event) =>
        event.officials.nodes.map((official) => ({
          ...official,
          event,
          federation: event.federation,
          date: event.startDate,
        })),
      ),
      ...(data?.officialCompetitions?.nodes ?? []).flatMap((competition) => {
        const event = competition.event;
        return event
          ? competition.officials.nodes.map((official) => ({
              ...official,
              event,
              federation: competition.federation,
              date: competition.startDate,
            }))
          : [];
      }),
    ];
    const competitions: CalendarCompetitionEvent[] = groupCompetitionEvents(
      competitionItems,
      officials,
    ).map(({ key, name, date, entries, officials }) => ({
      kind: 'competition',
      id: `competition:${key}`,
      title: name,
      ...calendarDay(date!),
      resourceKeys: effectiveGroupBy === 'none' ? [] : [resourceKey(competitionResource)],
      items: entries,
      officials,
    }));
    calendarEvents.push(...competitions);

    for (const person of data?.birthdays?.nodes ?? []) {
      if (person.date) {
        const date = person.date;
        calendarEvents.push({
          kind: 'birthday',
          id: `birthday:${person.id}:${date}`,
          person,
          date,
          ...calendarDay(date),
          resourceKeys: [],
        });
      }
    }

    if (effectiveGroupBy === 'trainer') {
      for (const trainer of trainers) {
        addResource({
          type: 'person',
          id: trainer.id,
          resourceTitle: trainer.name,
        });
      }
    }
    if (effectiveGroupBy !== 'none' && competitions.length > 0) {
      addResource(competitionResource);
    }
    return {
      range,
      events: data?.eventsList ?? [],
      calendarEvents,
      competitions,
      fetching,
      error,
      filterOptions,
      resources: [...resources.values()]
        .filter(
          (resource) =>
            effectiveGroupBy !== 'trainer' ||
            !operation?.variables.trainerIds ||
            resource.type !== 'person' ||
            operation.variables.trainerIds.includes(resource.id),
        )
        .toSorted((a, b) => compareText(a.resourceTitle, b.resourceTitle)),
    };
  }, [
    availableTrainers,
    withFacets,
    data,
    filters,
    showAllResources,
    auth,
    effectiveGroupBy,
    operation,
    range,
    fetching,
    error,
  ]);
}
