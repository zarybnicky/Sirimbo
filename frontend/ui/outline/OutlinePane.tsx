'use client';

import {
  OutlineDocument,
  SaveOutlineDocument,
  TenantOutlineDocument,
} from '@/graphql/Document';
import { useDebounced } from '@/lib/use-debounced';
import { FormError } from '@/ui/form';
import { useTagCandidates } from '@/ui/outline/candidates';
import { OutlineEditor } from '@/ui/outline/OutlineEditor';
import { nodeToRow, type OutlineRow } from '@/ui/outline/blocks';
import React from 'react';
import { useMutation, useQuery } from 'urql';

// Reads one subtree and writes it back scoped to that root, so editing a zoomed
// in node cannot disturb anything outside it. A rejected save means someone else
// moved the tree on; the version comes back with the error.
export function OutlinePane({ root, editable }: { root?: string; editable: boolean }) {
  const [{ data: tenant, fetching: loadingVersion }] = useQuery({
    query: TenantOutlineDocument,
  });
  const [{ data }, refetch] = useQuery({
    query: OutlineDocument,
    variables: { root: root ?? null },
  });
  const [saveState, save] = useMutation(SaveOutlineDocument);
  const candidates = useTagCandidates();
  const [rejected, setRejected] = React.useState<Error | null>(null);

  // A tenant that has never saved has no document yet, so there is no version to
  // disagree with; the first save creates one holding this.
  const version = tenant?.tenantDocument?.version ?? '1';

  const rows = React.useMemo<OutlineRow[]>(
    () => (data?.documentSubtreeList ?? []).map(nodeToRow),
    [data],
  );

  const persist = useDebounced(async (next: OutlineRow[]) => {
    if (loadingVersion) {
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
