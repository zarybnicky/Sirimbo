import { TagDancesDocument } from '@/graphql/Document';
import { CohortListDocument } from '@/graphql/Cohorts';
import { CoupleListDocument } from '@/graphql/Memberships';
import { FullPersonListDocument } from '@/graphql/Person';
import { formatCoupleName } from '@/ui/format';
import { rankItem } from '@tanstack/match-sorter-utils';
import React from 'react';
import { useQuery } from 'urql';
import type { TagKind } from './tag-inline.tsx';

export type TagCandidate = {
  kind: TagKind;
  refId: string;
  label: string;
};

const AT_KINDS = new Set(['person', 'couple', 'cohort']);
const HASH_KINDS = new Set(['discipline', 'dance', 'month']);

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

// Small enough to load once and rank in the browser, the way the rest of the app
// filters lists. Competitions are absent on purpose: they belong to the
// aggregator, which the club API does not expose.
export function useTagCandidates() {
  const [{ data: people }] = useQuery({ query: FullPersonListDocument });
  const [{ data: couples }] = useQuery({ query: CoupleListDocument });
  const [{ data: cohorts }] = useQuery({ query: CohortListDocument, variables: {} });
  const [{ data: dances }] = useQuery({ query: TagDancesDocument });

  const all = React.useMemo<TagCandidate[]>(
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
    (char: string, query: string) => {
      const kinds = char === '@' ? AT_KINDS : HASH_KINDS;
      const pool = all.filter((candidate) => kinds.has(candidate.kind));
      if (!query) {
        return pool.slice(0, 10);
      }
      return pool
        .map((item) => ({ item, ranking: rankItem(item.label, query) }))
        .filter((scored) => scored.ranking.passed)
        .toSorted((a, b) => b.ranking.rank - a.ranking.rank)
        .slice(0, 10)
        .map((scored) => scored.item);
    },
    [all],
  );
}
