'use client';

import {
  OutlineDocument,
  SaveOutlineDocument,
  TenantOutlineDocument,
} from '@/graphql/Document';
import { FormError } from '@/ui/form';
import { useTagCandidates } from '@/ui/fields/outline/candidates';
import { OutlineEditor } from '@/ui/fields/outline/OutlineEditor';
import { nodeToRow, type OutlineRow } from '@/ui/fields/outline/blocks';
import React from 'react';
import { useMutation, useQuery } from 'urql';

// Reads one subtree and writes it back scoped to that root, so editing a zoomed
// in node cannot disturb anything outside it. A rejected save means someone else
// moved the tree on; the version comes back with the error.
export function OutlinePane({ root, editable }: { root?: string; editable: boolean }) {
  const [{ data: tenant }] = useQuery({ query: TenantOutlineDocument });
  const [{ data }, refetch] = useQuery({
    query: OutlineDocument,
    variables: { root: root ?? null },
  });
  const [saveState, save] = useMutation(SaveOutlineDocument);
  const candidates = useTagCandidates();
  const [rejected, setRejected] = React.useState<Error | null>(null);

  const version = tenant?.tenantDocument?.version;

  const rows = React.useMemo<OutlineRow[]>(
    () =>
      (data?.documentSubtreeList ?? []).map(nodeToRow),
    [data],
  );

  const persist = useDebounced(async (next: OutlineRow[]) => {
    if (version === undefined) {
      return;
    }
    const result = await save({
      input: {
        root: root ?? null,
        baseVersion: version,
        nodes: next.map((row) => ({
          id: row.id,
          parentId: row.parentId,
          ordering: row.ordering.toString(),
          content: JSON.stringify(row.content),
        })),
      },
    });
    setRejected(result.error ?? null);
    if (result.error) {
      refetch({ requestPolicy: 'network-only' });
    }
  }, 800);

  if (!data) {
    return null;
  }

  return (
    <>
      <FormError error={rejected} />
      <OutlineEditor
        key={root ?? 'all'}
        rows={rows}
        editable={editable}
        onChange={editable ? persist : undefined}
        candidates={editable ? candidates : undefined}
      />
      {saveState.fetching && <p className="mt-1 text-xs text-neutral-10">Ukládám…</p>}
    </>
  );
}

// Unmounting has to flush rather than cancel: navigating away within the
// debounce window would otherwise drop whatever was typed last.
function useDebounced<T>(fn: (value: T) => void | Promise<void>, delay: number) {
  const timer = React.useRef<ReturnType<typeof setTimeout>>(undefined);
  const pending = React.useRef<{ value: T } | null>(null);
  const latest = React.useRef(fn);

  React.useEffect(() => {
    latest.current = fn;
  });

  React.useEffect(
    () => () => {
      clearTimeout(timer.current);
      if (pending.current) {
        void latest.current(pending.current.value);
        pending.current = null;
      }
    },
    [],
  );

  return React.useCallback(
    (value: T) => {
      clearTimeout(timer.current);
      pending.current = { value };
      timer.current = setTimeout(() => {
        pending.current = null;
        void latest.current(value);
      }, delay);
    },
    [delay],
  );
}
