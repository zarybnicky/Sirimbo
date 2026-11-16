import { useMemo } from 'react';
import { useQuery } from 'urql';
import {
  createLoader,
  parseAsBoolean,
  parseAsString,
  parseAsStringLiteral,
  type inferParserType,
} from 'nuqs';
import type { PersonMembershipCondition } from '@/graphql';
import { PersonListDocument } from '@/graphql/Person';
import { compareText } from '@/lib/compare';
import { useFuzzySearch } from '@/lib/useFuzzySearch';

export const personFilterParsers = {
  search: parseAsString.withDefault(''),
  cohortId: parseAsString,
  membership: parseAsStringLiteral(['current', 'former']).withDefault('current'),
  isTrainer: parseAsBoolean.withDefault(false),
  isAdmin: parseAsBoolean.withDefault(false),
};
export const loadPersonFilters = createLoader(personFilterParsers);
export type PersonFilters = inferParserType<typeof personFilterParsers>;

export function usePersonData({
  filters,
  withFacets = true,
  withActivity = false,
  withCompetitive = false,
}: {
  filters: PersonFilters;
  withFacets?: boolean;
  withActivity?: boolean;
  withCompetitive?: boolean;
}) {
  const { cohortId, membership, isTrainer, isAdmin, search } = filters;
  const condition: PersonMembershipCondition = {
    inCohorts: cohortId === 'none' ? [] : cohortId ? [cohortId] : null,
    isTrainer: isTrainer || null,
    isAdmin: isAdmin || null,
    state: membership === 'former' ? 'FORMER' : 'CURRENT',
  };
  const [{ data, fetching, error }] = useQuery({
    query: PersonListDocument,
    variables: {
      condition: { membership: condition },
      withActivity,
      withCompetitive,
      withFacets,
      inCohorts: condition.inCohorts,
      isTrainer: condition.isTrainer,
      isAdmin: condition.isAdmin,
      membershipStates: [membership],
    },
  });
  const rows = useMemo(
    () =>
      (data?.people?.nodes ?? [])
        .toSorted(
          (a, b) =>
            compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
        )
        .map((person) => {
          const active = person.cohortMembershipsList.filter(
            (x) => x.status === 'ACTIVE',
          );
          const cohortMembership = active.find((x) => x.cohort?.id === cohortId);
          return {
            ...person,
            ageGroup: person.ageGroup ?? null,
            federationCategoryProgresses: person.federationCategoryProgresses ?? {
              nodes: [],
            },
            yearOfBirth: person.birthDate
              ? new Date(person.birthDate).getFullYear()
              : undefined,
            cohortNames: active.flatMap((x) => x.cohort?.name ?? []).join(', '),
            cohortColors: active.flatMap((x) => (x.cohort ? [x.cohort.colorRgb] : [])),
            cohortMembership: cohortMembership
              ? { ...cohortMembership, person: { name: person.name } }
              : undefined,
          };
        }),
    [data, cohortId],
  );
  const people = useFuzzySearch(
    rows,
    ['id', 'name', 'cohortNames', 'phone', 'email', 'yearOfBirth'],
    search,
    Infinity,
  );
  const counts: Record<string, Record<string, number>> = {};
  for (const { facet, value, count } of data?.personFacetsList ?? []) {
    if (facet && value) (counts[facet] ??= {})[value] = count ?? 0;
  }
  return {
    people,
    fetching,
    error,
    filterOptions: {
      cohorts: [
        {
          id: 'none',
          label: 'Bez skupiny',
          count: data?.personFacetsList ? (counts.cohort?.none ?? 0) : undefined,
        },
        ...(data?.cohortsList ?? []).map((x) => ({
          id: x.id,
          label: x.name,
          count: data?.personFacetsList ? (counts.cohort?.[x.id] ?? 0) : undefined,
        })),
      ],
      flags: [
        {
          value: 'former',
          label: 'Bývalí',
          count: data?.personFacetsList ? (counts.state?.former ?? 0) : undefined,
        },
        {
          value: 'trainer',
          label: 'Trenéři',
          count: data?.personFacetsList ? (counts.trainer?.true ?? 0) : undefined,
        },
        {
          value: 'admin',
          label: 'Správci',
          count: data?.personFacetsList ? (counts.admin?.true ?? 0) : undefined,
        },
      ],
    },
  };
}
