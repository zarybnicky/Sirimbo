import {
  EventFormOptionsDocument,
  type EventFormOptionsQueryVariables,
} from '@/graphql/Event';
import { compareText } from '@/lib/compare';
import { fullDateFormatter } from '@/ui/format';
import React from 'react';
import { useQuery } from 'urql';
import type { EventParticipantOption } from './types';

export function useEventFormOptions(variables: EventFormOptionsQueryVariables) {
  const [query] = useQuery({ query: EventFormOptionsDocument, variables });
  const savedRegistrations = query.data?.registrations;
  const participants = React.useMemo(() => {
    const tenant = query.data?.tenant;
    const participants = new Map<string, EventParticipantOption>();

    for (const { person } of [
      ...(savedRegistrations ?? []),
      ...(query.data?.people?.nodes ?? []).map((person) => ({ person })),
      ...(tenant?.cohortsList ?? []).flatMap((x) => x.cohortMembershipsList),
      ...(tenant?.tenantMembershipsList ?? []),
    ]) {
      if (!person) continue;
      participants.set(`person:${person.id}`, {
        id: `person:${person.id}`,
        label: person.name + (person.isExternal ? ' (Externí)' : ''),
        isExternal: person.isExternal,
        personId: person.id,
        coupleId: null,
        personIds: [person.id],
        person,
        couple: null,
      });
    }
    for (const couple of [
      ...(savedRegistrations ?? []).flatMap((x) => x.couple || []),
      ...(query.data?.couplesList ?? []),
    ]) {
      participants.set(`couple:${couple.id}`, {
        id: `couple:${couple.id}`,
        label: couple.name,
        isExternal: ![couple.manId, couple.womanId].some(
          (id) => !participants.get(`person:${id}`)?.isExternal,
        ),
        personId: null,
        coupleId: couple.id,
        personIds: [couple.manId, couple.womanId],
        person: null,
        couple,
      });
    }
    return new Map(
      [...participants].toSorted(
        ([, a], [, b]) =>
          Number(a.isExternal) - Number(b.isExternal) || compareText(a.label, b.label),
      ),
    );
  }, [
    query.data?.couplesList,
    query.data?.tenant,
    query.data?.people,
    savedRegistrations,
  ]);

  const facets = React.useMemo(
    () => ({
      locations:
        query.data?.tenant?.locationsList?.map((location) => ({
          id: location.id,
          label: location.name,
        })) ?? [],
      cohorts:
        query.data?.tenant?.cohortsList
          ?.filter((cohort) => !cohort.isArchived)
          .map((cohort) => ({ id: cohort.id, label: cohort.name })) ?? [],
      parents: [
        { id: null, label: 'Bez omezení' },
        ...(query.data?.parents?.map((event) => ({
          id: event.id,
          label: `${event.name || 'Soustředění'} – ${fullDateFormatter.formatRange(
            new Date(event.since),
            new Date(event.until),
          )}`,
        })) ?? []),
      ],
    }),
    [query.data],
  );

  return { ...query, participants, ...facets };
}
