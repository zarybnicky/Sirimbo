'use client';

import { DocumentDocument, UpsertDocumentDocument } from '@/graphql/Document';
import { useTagCandidates } from '@/ui/fields/outline/candidates';
import { OutlineEditor } from '@/ui/fields/outline/OutlineEditor';
import type { OutlineRow } from '@/ui/fields/outline/rows';
import React from 'react';
import { useMutation, useQuery } from 'urql';

export type DocumentSubject =
  | { eventInstanceId: string }
  | { eventSeriesId: string }
  | { cohortId: string };

export function DocumentPane({ id }: { id: string }) {
  const [{ data }] = useQuery({ query: DocumentDocument, variables: { id } });
  const [, upsert] = useMutation(UpsertDocumentDocument);
  const candidates = useTagCandidates();
  const doc = data?.document;

  const rows = React.useMemo<OutlineRow[]>(
    () =>
      (doc?.nodesList ?? []).map((node) => ({
        id: node.id,
        parentId: node.parentId ?? null,
        ordering: Number(node.ordering),
        content: node.content,
      })),
    [doc],
  );

  const save = useDebounced((next: OutlineRow[]) => {
    if (!doc) {
      return;
    }
    void upsert({
      input: {
        doc: {
          id: doc.id,
          title: doc.title,
          eventInstanceId: doc.eventInstanceId,
          eventSeriesId: doc.eventSeriesId,
          cohortId: doc.cohortId,
          showToMembers: doc.showToMembers,
        },
        nodes: next.map((row) => ({
          id: row.id,
          parentId: row.parentId,
          ordering: row.ordering.toString(),
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

export function useCreateDocument(subject: DocumentSubject) {
  const [{ fetching }, upsert] = useMutation(UpsertDocumentDocument);

  const create = React.useCallback(
    async (title: string) => {
      const result = await upsert({ input: { doc: { title, ...subject }, nodes: [] } });
      return result.data?.upsertDocument?.document?.id;
    },
    [subject, upsert],
  );

  return [fetching, create] as const;
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
