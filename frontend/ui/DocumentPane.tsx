'use client';

import { type DocumentFragment, DocumentDocument, UpsertDocumentDocument } from '@/graphql/Document';
import { useAuth } from '@/lib/auth';
import { useTagCandidates } from '@/ui/fields/outline/candidates';
import { OutlineEditor } from '@/ui/fields/outline/OutlineEditor';
import type { OutlineRow } from '@/ui/fields/outline/rows';
import React from 'react';
import { useMutation, useQuery } from 'urql';

export type DocumentSubject =
  | { eventInstanceId: string }
  | { eventSeriesId: string }
  | { cohortId: string };

// Trainers and administrators may write; a member reading a shared plan gets the
// same rendering without an editor, a save path or the candidate queries.
export function DocumentPane({ id }: { id: string }) {
  const auth = useAuth();
  return auth.isTrainerOrAdmin ? <DocumentEditor id={id} /> : <DocumentView id={id} />;
}

function DocumentView({ id }: { id: string }) {
  const { doc, rows } = useDocumentRows(id);
  if (!doc) {
    return null;
  }
  return <OutlineEditor key={id} rows={rows} editable={false} />;
}

function DocumentEditor({ id }: { id: string }) {
  const { doc, rows } = useDocumentRows(id);
  const [, upsert] = useMutation(UpsertDocumentDocument);
  const candidates = useTagCandidates();

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

function useDocumentRows(id: string) {
  const [{ data }] = useQuery({ query: DocumentDocument, variables: { id } });
  const doc = data?.document as DocumentFragment | null | undefined;

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

  return { doc, rows };
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
