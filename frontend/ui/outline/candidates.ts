import { CohortListDocument } from '@/graphql/Cohorts';
import { TagDancesDocument, TagEventCandidatesDocument } from '@/graphql/Document';
import { CoupleListDocument } from '@/graphql/Memberships';
import { FullPersonListDocument } from '@/graphql/Person';
import { formatCoupleName, formatEventCandidate } from '@/ui/format';
import { rankItem } from '@tanstack/match-sorter-utils';
import React from 'react';
import { useClient, useQuery } from 'urql';
import type { TagKind } from './tags.tsx';

export type TagCandidate = {
  kind: TagKind;
  refId: string;
  label: string;
};

const LIMIT = 10;

// `@` picks out someone or something with a page of its own; `#` classifies. The
// two sets are disjoint, which is what makes one menu per trigger character work.
const AT_KINDS = new Set<TagKind>(['person', 'couple', 'cohort', 'event', 'series']);
const HASH_KINDS = new Set<TagKind>(['discipline', 'dance', 'month']);

// Neither needs a lookup: a fixed vocabulary, and the months around today.
const DISCIPLINE_CANDIDATES: TagCandidate[] = [
  { kind: 'discipline', refId: 'stt', label: 'STT' },
  { kind: 'discipline', refId: 'lat', label: 'LAT' },
  { kind: 'discipline', refId: 'conditioning', label: 'Kondice' },
  { kind: 'discipline', refId: 'practice', label: 'Praxe' },
];

export function monthCandidates(today: Date, span = 12): TagCandidate[] {
  return Array.from({ length: span }, (_, offset) => {
    const date = new Date(Date.UTC(today.getFullYear(), today.getMonth() - offset, 1));
    const refId = date.toISOString().slice(0, 10);
    return { kind: 'month' as const, refId, label: refId.slice(0, 7) };
  });
}

export function rankCandidates(pool: TagCandidate[], query: string): TagCandidate[] {
  if (!query) {
    return pool.slice(0, LIMIT);
  }
  return pool
    .map((item) => ({ item, ranking: rankItem(item.label, query) }))
    .filter((scored) => scored.ranking.passed)
    .toSorted((a, b) => b.ranking.rank - a.ranking.rank)
    .slice(0, LIMIT)
    .map((scored) => scored.item);
}

// People, couples, cohorts and dances are small enough to load once and rank in
// the browser, the way the rest of the app filters lists. Events are not — there
// are tens of thousands — so they are searched server side as the menu is typed
// into. Competitions are absent on purpose: they belong to the aggregator, which
// the club API does not expose.
export function useTagCandidates() {
  const client = useClient();
  const [{ data: people }] = useQuery({ query: FullPersonListDocument });
  const [{ data: couples }] = useQuery({ query: CoupleListDocument });
  const [{ data: cohorts }] = useQuery({ query: CohortListDocument, variables: {} });
  const [{ data: dances }] = useQuery({ query: TagDancesDocument });

  const loaded = React.useMemo<TagCandidate[]>(
    () => [
      ...(people?.people?.nodes ?? []).map((person) => ({
        kind: 'person' as const,
        refId: person.id,
        label: person.name,
      })),
      ...(couples?.getCurrentTenant?.couplesList ?? []).map((couple) => ({
        kind: 'couple' as const,
        refId: couple.id,
        label: formatCoupleName(couple),
      })),
      ...(cohorts?.cohortsList ?? []).map((cohort) => ({
        kind: 'cohort' as const,
        refId: cohort.id,
        label: cohort.name,
      })),
      ...(dances?.dancesList ?? []).map((dance) => ({
        kind: 'dance' as const,
        refId: dance.code,
        label: dance.name ?? dance.code,
      })),
      ...DISCIPLINE_CANDIDATES,
      ...monthCandidates(new Date()),
    ],
    [people, couples, cohorts, dances],
  );

  return React.useCallback(
    async (char: string, query: string): Promise<TagCandidate[]> => {
      const kinds = char === '@' ? AT_KINDS : HASH_KINDS;
      const ranked = rankCandidates(
        loaded.filter((candidate) => kinds.has(candidate.kind)),
        query,
      );
      if (!kinds.has('event')) {
        return ranked;
      }

      // Already ordered and limited by the database, so these go in front of the
      // browser-ranked ones rather than being re-ranked against them.
      const { data } = await client
        .query(TagEventCandidatesDocument, { query, count: LIMIT })
        .toPromise();

      return [
        ...(data?.eventSeriesCandidatesList ?? []).map((series) => ({
          kind: 'series' as const,
          refId: series.id,
          label: series.name ?? `Série ${series.id}`,
        })),
        ...(data?.eventInstanceCandidatesList ?? []).map((instance) => ({
          kind: 'event' as const,
          refId: instance.id,
          label: formatEventCandidate(instance),
        })),
        ...ranked,
      ].slice(0, LIMIT * 2);
    },
    [client, loaded],
  );
}
