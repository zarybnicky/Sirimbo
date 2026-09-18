'use client';

import {
  CohortDocumentsDocument,
  DocumentDocument,
  EventInstanceDocumentsDocument,
  EventSeriesDocumentsDocument,
  UpsertDocumentDocument,
} from '@/graphql/Document';
import { CohortListDocument } from '@/graphql/Cohorts';
import { FullPersonListDocument } from '@/graphql/Person';
import { TabMenu } from '@/ui/TabMenu';
import { OutlineEditor } from '@/ui/fields/outline/OutlineEditor';
import type { OutlineRow } from '@/ui/fields/outline/rows';
import {
  DISCIPLINE_CANDIDATES,
  monthCandidates,
  type TagCandidate,
} from '@/ui/fields/outline/suggestion';
import { rankItem } from '@tanstack/match-sorter-utils';
import React from 'react';
import { useMutation, useQuery } from 'urql';

export type DocumentSubject =
  | { eventInstanceId: string }
  | { eventSeriesId: string }
  | { cohortId: string };

export function DocumentTabs({ subject }: { subject: DocumentSubject }) {
  const [selected, setSelected] = React.useState<string | null>(null);
  const [, upsert] = useMutation(UpsertDocumentDocument);
  const candidates = useTagCandidates();

  const byInstance = 'eventInstanceId' in subject;
  const bySeries = 'eventSeriesId' in subject;

  const [instanceDocs, refetchInstance] = useQuery({
    query: EventInstanceDocumentsDocument,
    variables: { instanceId: byInstance ? subject.eventInstanceId : '' },
    pause: !byInstance,
  });
  const [seriesDocs, refetchSeries] = useQuery({
    query: EventSeriesDocumentsDocument,
    variables: { seriesId: bySeries ? subject.eventSeriesId : '' },
    pause: !bySeries,
  });
  const [cohortDocs, refetchCohort] = useQuery({
    query: CohortDocumentsDocument,
    variables: { cohortId: 'cohortId' in subject ? subject.cohortId : '' },
    pause: byInstance || bySeries,
  });

  const list = byInstance ? instanceDocs : bySeries ? seriesDocs : cohortDocs;
  const refetchList = byInstance ? refetchInstance : bySeries ? refetchSeries : refetchCohort;
  const documents = React.useMemo(() => list.data?.documents?.nodes ?? [], [list.data]);

  const create = React.useCallback(async () => {
    const result = await upsert({
      input: { doc: { kind: 'plan', title: 'Nový plán', ...subject }, nodes: [] },
    });
    const created = result.data?.upsertDocument?.document;
    if (created) {
      setSelected(created.id);
      refetchList({ requestPolicy: 'network-only' });
    }
  }, [subject, upsert, refetchList]);

  const options = React.useMemo(
    () =>
      documents.map((doc) => ({
        id: doc.id,
        title: doc.title || 'Bez názvu',
        contents: () => <DocumentPane id={doc.id} subject={subject} candidates={candidates} />,
      })),
    [documents, subject, candidates],
  );

  return (
    <div>
      <div className="flex items-end gap-2">
        <TabMenu options={options} selected={selected} onSelect={setSelected} className="grow" />
        <button
          type="button"
          onClick={create}
          className="mb-2 rounded px-2 py-1 text-sm text-accent-11 hover:bg-neutral-4"
          title="Přidat dokument"
        >
          +
        </button>
      </div>
      {options.length === 0 && (
        <p className="py-4 text-sm text-neutral-11">Zatím tu není žádný plán.</p>
      )}
    </div>
  );
}

function DocumentPane({
  id,
  subject,
  candidates,
}: {
  id: string;
  subject: DocumentSubject;
  candidates: (char: string, query: string) => TagCandidate[];
}) {
  const [{ data }] = useQuery({ query: DocumentDocument, variables: { id } });
  const [, upsert] = useMutation(UpsertDocumentDocument);
  const doc = data?.document;

  const rows = React.useMemo<OutlineRow[]>(
    () =>
      (doc?.nodes?.nodes ?? []).map((node) => ({
        id: node.id,
        parentId: node.parentId ?? null,
        ordering: Number(node.ordering),
        listType: node.listType === 'ordered' ? 'ordered' : 'bullet',
        content: node.content,
      })),
    [doc],
  );

  const save = useDebounced((next: OutlineRow[]) => {
    void upsert({
      input: {
        doc: { id, kind: doc?.kind ?? 'plan', title: doc?.title ?? null, ...subject },
        nodes: next.map((row) => ({
          id: row.id,
          parentId: row.parentId,
          ordering: row.ordering.toString(),
          listType: row.listType,
          content: row.content,
        })),
      },
    });
  }, 800);

  if (!doc) {
    return null;
  }

  return <OutlineEditor key={id} rows={rows} onChange={save} candidates={candidates} />;
}

function useDebounced<T>(fn: (value: T) => void, delay: number) {
  const timer = React.useRef<ReturnType<typeof setTimeout>>(undefined);
  const latest = React.useRef(fn);

  React.useEffect(() => {
    latest.current = fn;
  });
  React.useEffect(() => () => clearTimeout(timer.current), []);

  return React.useCallback(
    (value: T) => {
      clearTimeout(timer.current);
      timer.current = setTimeout(() => latest.current(value), delay);
    },
    [delay],
  );
}

// People and cohorts are small enough to load once and rank in the browser, the
// way the rest of the app filters lists. Dances and competitions live in the
// federated schema, which the API does not expose, so they are not offered yet.
function useTagCandidates() {
  const [{ data: people }] = useQuery({ query: FullPersonListDocument });
  const [{ data: cohorts }] = useQuery({ query: CohortListDocument, variables: {} });

  const all = React.useMemo<TagCandidate[]>(
    () => [
      ...(people?.people?.nodes ?? []).map((person) => ({
        kind: 'person' as const,
        refId: person.id,
        label: person.name,
      })),
      ...(cohorts?.cohortsList ?? []).map((cohort) => ({
        kind: 'cohort' as const,
        refId: cohort.id,
        label: cohort.name,
      })),
      ...DISCIPLINE_CANDIDATES,
      ...monthCandidates(new Date()),
    ],
    [people, cohorts],
  );

  return React.useCallback(
    (char: string, query: string) => {
      const kinds =
        char === '@' ? new Set(['person', 'couple', 'cohort', 'event']) : new Set(['discipline', 'month']);
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
