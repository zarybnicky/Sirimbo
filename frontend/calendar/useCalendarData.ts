import {
  CompetitionBriefDocument,
  CompetitionReportDocument,
} from '@/graphql/Federation';
import { type CompetitionEntry, competitionEntryKey } from '@/ui/Competitions';
import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
  type EventInstanceRangeQueryVariables,
} from '@/graphql/Event';
import { BirthdayPeopleDocument } from '@/graphql/Person';
import { add, startOf } from 'date-arithmetic';
import React from 'react';
import { useClient, useQuery } from 'urql';
import type { CalendarEvent, CalendarInstanceEvent, DateRange, Resource } from './types';
import { CalendarView } from '@/calendar/CalendarViews';
import { localDateKey } from '@/calendar/localizer';
import { mapBirthdayPeopleToCalendar } from '@/calendar/birthdays';

export type CalendarGroupBy = 'none' | 'trainer' | 'room';
export type CalendarFilters = {
  participantIds: string[];
  trainerIds: string[];
  onlyMine: boolean;
  myPersonIds: string[];
};

const getResourceKey = (type: string, id: string) => `${type}:${id}`;
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
  { onlyMine, trainerIds, participantIds }: CalendarFilters,
): EventInstanceRangeQueryVariables {
  return {
    start: startOf(range.since, 'day').toISOString(),
    end: add(startOf(range.until, 'day'), 1, 'day').toISOString(),
    trainerIds: trainerIds.length > 0 ? trainerIds : undefined,
    participantIds: participantIds.length > 0 ? participantIds : undefined,
    onlyMine,
  };
}

function mapInstancesToCalendar(
  list: EventInstanceRangeQuery['list'],
  groupBy: CalendarGroupBy,
): { events: CalendarInstanceEvent[]; resources: Resource[] } {
  const events: CalendarInstanceEvent[] = [];
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
    events.push({
      kind: 'event',
      event,
      instance,
      resourceIds: eventResourceIds,
      start,
      end,
    });
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
  const effectiveGroupBy = filters.onlyMine ? 'none' : groupBy;
  const birthdayRange = React.useMemo(() => {
    const since = startOf(range.since, 'day');
    const until = add(startOf(range.until, 'day'), 1, 'day');

    return {
      since: localDateKey(since),
      until: localDateKey(until),
    };
  }, [range.since, range.until]);
  const competitionRanges = React.useMemo(() => {
    const since = startOf(range.since, 'day');
    const until = add(startOf(range.until, 'day'), 1, 'day');
    const today = startOf(new Date(), 'day');
    const tomorrow = add(today, 1, 'day');
    const reportUntil = new Date(Math.min(until.getTime(), tomorrow.getTime()));
    const briefSince = new Date(Math.max(since.getTime(), today.getTime()));

    return {
      report: since < reportUntil ? { since, until: reportUntil } : null,
      brief: briefSince < until ? { since: briefSince, until } : null,
    };
  }, [range.since, range.until]);
  const competitionPersonIds =
    filters.participantIds.length > 0
      ? filters.participantIds
      : filters.onlyMine
        ? filters.myPersonIds
        : undefined;
  const [{ data: reportData }] = useQuery({
    query: CompetitionReportDocument,
    variables: competitionRanges.report
      ? {
          since: localDateKey(competitionRanges.report.since),
          until: localDateKey(competitionRanges.report.until),
          personIds: competitionPersonIds,
        }
      : {},
    pause: !competitionRanges.report,
  });
  const [{ data: briefData }] = useQuery({
    query: CompetitionBriefDocument,
    variables: competitionRanges.brief
      ? {
          since: localDateKey(competitionRanges.brief.since),
          until: localDateKey(competitionRanges.brief.until),
          personIds: competitionPersonIds,
        }
      : {},
    pause: !competitionRanges.brief,
  });
  const birthdayPersonIds =
    filters.participantIds.length > 0
      ? filters.participantIds
      : filters.onlyMine
        ? filters.myPersonIds
        : undefined;
  const [{ data: birthdayData, fetching: birthdaysFetching }] = useQuery({
    query: BirthdayPeopleDocument,
    variables: birthdayRange,
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

  const { events, resources } = React.useMemo(() => {
    const schedule = mapInstancesToCalendar(data?.list ?? null, effectiveGroupBy);
    const competitionsByEvent = new Map<string, CompetitionBucket>();
    const seen = new Set<string>();
    const addCompetition = (item: CompetitionEntry) => {
      const key = competitionEntryKey(item);
      if (!item.competitionDate || seen.has(key)) return;
      seen.add(key);

      const eventKey = `${item.competitionDate}:${item.eventId ?? ''}`;
      const competition = competitionsByEvent.get(eventKey) ?? {
        date: item.competitionDate,
        title: item.eventName ?? 'Soutěže',
        eventLocation: item.eventLocation,
        items: [],
      };
      competition.items.push(item);
      competitionsByEvent.set(eventKey, competition);
    };

    for (const row of reportData?.competitionReportList ?? []) {
      addCompetition({ ...row, kind: 'report' });
    }
    for (const row of briefData?.competitionBriefList ?? []) {
      addCompetition({ ...row, kind: 'brief' });
    }

    const competitions: CalendarEvent[] = [...competitionsByEvent.values()].map(
      ({ date, items, title, eventLocation }) => ({
        kind: 'competition',
        id: `competition:${date}:${items[0]?.eventId ?? 'unknown'}`,
        title,
        eventLocation,
        start: new Date(`${date}T00:00:00`),
        end: new Date(`${date}T23:59:00`),
        resourceIds: effectiveGroupBy === 'none' ? [] : [competitionResource.resourceId],
        isDraggable: false,
        isResizable: false,
        items: items.toSorted(
          (a, b) =>
            a.kind.localeCompare(b.kind) ||
            (a.competitorName ?? '').localeCompare(b.competitorName ?? '', 'cs') ||
            (a.category?.name ?? '').localeCompare(b.category?.name ?? '', 'cs'),
        ),
      }),
    );
    const birthdays = mapBirthdayPeopleToCalendar(
      birthdayData?.people?.nodes ?? [],
      birthdayRange.since,
      birthdayRange.until,
      birthdayPersonIds,
    );

    return {
      events: [...schedule.events, ...competitions, ...birthdays],
      resources:
        effectiveGroupBy !== 'none' && competitions.length > 0
          ? [...schedule.resources, competitionResource]
          : schedule.resources,
    };
  }, [
    briefData?.competitionBriefList,
    birthdayData?.people?.nodes,
    birthdayPersonIds,
    birthdayRange.since,
    birthdayRange.until,
    data?.list,
    effectiveGroupBy,
    reportData?.competitionReportList,
  ]);

  return {
    range,
    events,
    resources,
    fetching: fetching || birthdaysFetching,
  };
}
