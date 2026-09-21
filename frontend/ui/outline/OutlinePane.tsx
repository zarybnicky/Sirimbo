'use client';

import { OutlineDocument, SaveOutlineDocument } from '@/graphql/Document';
import { useDebounced } from '@/lib/use-debounced';
import { FormError } from '@/ui/form';
import { OutlineEditor } from '@/ui/outline/OutlineEditor';
import { nodeToRow, type OutlineRow } from '@/ui/outline/blocks';
import { useTagCandidates } from '@/ui/outline/candidates';
import { uploadOutlineFile } from '@/ui/outline/upload';
import React from 'react';
import { useMutation, useQuery } from 'urql';

// Reads one subtree and writes back the nodes the editor is holding. Each carries
// the version it was loaded at, so two people working on different parts of the
// outline do not collide; a node the editor never rendered is simply absent, and
// absence means nothing — deletions are named.
export function OutlinePane({ root, editable }: { root?: string; editable: boolean }) {
  const [{ data }, refetch] = useQuery({
    query: OutlineDocument,
    variables: { root: root ?? null },
  });
  const [saveState, save] = useMutation(SaveOutlineDocument);
  const candidates = useTagCandidates();
  const [rejected, setRejected] = React.useState<Error | null>(null);
  // Bumped when a save is rejected, to remount the editor on the fresh tree.
  const [reload, setReload] = React.useState(0);

  const rows = React.useMemo<OutlineRow[]>(
    () => (data?.documentSubtreeList ?? []).map(nodeToRow),
    [data],
  );

  // What the editor was last known to hold, which is what a save is judged
  // against: the version of each node, and the ids that going missing would mean
  // a deletion. BlockNote carries neither back out.
  const held = React.useRef(new Map<string, string | null>());
  React.useEffect(() => {
    held.current = new Map(rows.map((row) => [row.id, row.version]));
  }, [rows]);

  // A zoomed-in root is rendered top level because its parent is outside the
  // excerpt, so its real place in the tree is put back here.
  const rootParentId = rows.find((row) => row.id === root)?.parentId ?? null;

  const persist = useDebounced(async (next: OutlineRow[]) => {
    const present = new Set(next.map((row) => row.id));
    const result = await save({
      input: {
        root: root ?? null,
        deleted: [...held.current.keys()].filter((id) => !present.has(id)),
        nodes: next.map((row) => ({
          id: row.id,
          parentId: row.id === root ? rootParentId : row.parentId,
          ordering: row.ordering.toString(),
          content: JSON.stringify(row.content),
          version: held.current.get(row.id) ?? null,
        })),
      },
    });

    setRejected(result.error ?? null);
    if (result.error) {
      // A genuine conflict: someone changed a node this editor was holding. The
      // fresh tree wins, so the edit that lost is visible in the error rather
      // than quietly overwriting theirs.
      refetch({ requestPolicy: 'network-only' });
      setReload((seen) => seen + 1);
      return;
    }

    const saved = result.data?.saveOutline?.documentNodes ?? [];
    held.current = new Map([
      ...saved.map((node) => [node.id, node.version] as const),
      ...[...held.current].filter(([id]) => present.has(id)),
    ]);
  }, 800);

  if (!data) {
    return null;
  }

  return (
    <>
      <FormError error={rejected} />
      <OutlineEditor
        key={`${root ?? 'all'}:${reload}`}
        rows={rows}
        editable={editable}
        onChange={editable ? persist : undefined}
        candidates={editable ? candidates : undefined}
        uploadFile={editable ? uploadOutlineFile : undefined}
      />
      {saveState.fetching && <p className="mt-1 text-xs text-neutral-10">Ukládám…</p>}
    </>
  );
}
