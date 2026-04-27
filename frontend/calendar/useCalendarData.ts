import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
  type EventInstanceRangeQueryVariables,
} from '@/graphql/Event';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useClient, useQuery } from 'urql';
import type { CalendarEvent, Resource } from './types';
import { CalendarView } from '@/calendar/CalendarViews';

export type CalendarGroupBy = 'none' | 'trainer' | 'room';
export type CalendarFilters = {
  trainerIds: string[];
  onlyMine: boolean;
};

const getResourceKey = (type: string, id: string) => `${type}:${id}`;

function prepareVariables(
  range: { start: Date; end: Date },
  filters: CalendarFilters,
): EventInstanceRangeQueryVariables {
  return {
    start: startOf(range.start, 'day').toISOString(),
    end: add(startOf(range.end, 'day'), 1, 'day').toISOString(),
    trainerIds: filters.trainerIds.length > 0 ? filters.trainerIds : undefined,
    onlyMine: filters.onlyMine,
  };
}

function mapInstancesToCalendar(
  list: EventInstanceRangeQuery['list'],
  groupBy: CalendarGroupBy,
): { events: CalendarEvent[]; resources: Resource[] } {
  const events: CalendarEvent[] = [];
  const resourceMap = new Map<string, Resource>();

  const put = (resource: Resource) => resourceMap.set(resource.resourceId, resource);

  for (const instance of list ?? []) {
    const { event } = instance ?? {};
    if (!event) continue;

    const start = new Date(instance.since);
    const end = new Date(instance.until);

    const eventResourceIds: string[] = [];

    if (groupBy === 'trainer') {
      for (const trainer of instance.trainersList ?? []) {
        const id = trainer.personId;
        if (!id) continue;
        eventResourceIds.push(getResourceKey('person', id));
        put({
          resourceId: getResourceKey('person', id),
          resourceTitle: trainer.person?.name || '',
        });
      }
    } else if (groupBy === 'room') {
      if (event.location?.id) {
        eventResourceIds.push(getResourceKey('location', event.location.id));
        put({
          resourceId: getResourceKey('location', event.location.id),
          resourceTitle: event.location.name,
        });
      } else if (event.locationText) {
        eventResourceIds.push(getResourceKey('locationText', event.locationText));
        put({
          resourceId: getResourceKey('locationText', event.locationText),
          resourceTitle: event.locationText,
        });
      }
    }
    if (groupBy !== 'none' && eventResourceIds.length === 0) {
      eventResourceIds.push('');
      put({ resourceId: '', resourceTitle: '-' });
    }
    events.push({ event, instance, resourceIds: eventResourceIds, start, end });
  }

  const resources = [...resourceMap.values()].toSorted((a, b) =>
    a.resourceTitle.localeCompare(b.resourceTitle),
  );

  return { events, resources };
}

export function useCalendarData(
  view: CalendarView,
  date: Date,
  filters: CalendarFilters,
  groupBy: CalendarGroupBy,
) {
  const client = useClient();

  const { range, variables: vars } = React.useMemo(() => {
    const range = view.range(date);
    const prev = view.range(view.nav(date, -1));
    const next = view.range(view.nav(date, 1));

    return {
      range,
      variables: {
        current: prepareVariables(range, filters),
        prev: prepareVariables(prev, filters),
        next: prepareVariables(next, filters),
      },
    };
  }, [view, date, filters]);

  const [{ data, fetching }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: vars.current,
    requestPolicy: 'cache-and-network',
  });

  React.useEffect(() => {
    const timeout = setTimeout(() => {
      client
        .query(EventInstanceRangeDocument, vars.prev, { requestPolicy: 'cache-first' })
        .toPromise();
      client
        .query(EventInstanceRangeDocument, vars.next, { requestPolicy: 'cache-first' })
        .toPromise();
    }, 100);

    return () => clearTimeout(timeout);
  }, [client, vars.prev, vars.next]);

  const effectiveGroupBy = filters.onlyMine ? 'none' : groupBy;
  const { events, resources } = React.useMemo(() => {
    return mapInstancesToCalendar(data?.list ?? null, effectiveGroupBy);
  }, [data?.list, effectiveGroupBy]);

  return {
    range,
    variables: vars.current,
    events,
    resources,
    fetching,
  };
}
