'use client';

import { AddOutlineNodeDocument, type DocumentMentionFragment } from '@/graphql/Document';
import { useAuth } from '@/lib/auth';
import { OutlineBreadcrumb } from '@/ui/outline/OutlineBreadcrumb';
import { OutlineEditor } from '@/ui/outline/OutlineEditor';
import { nodeToRow } from '@/ui/outline/blocks';
import type { TagKind } from '@/ui/outline/tags';
import { buttonCls, typographyCls } from '@/ui/style';
import Link from 'next/link';
import React from 'react';
import { useMutation } from 'urql';

export type OutlineSubject = {
  kind: Extract<TagKind, 'cohort' | 'event' | 'series'>;
  id: string;
  label: string;
};

// The tags come from the tagged entity's own query, so this only arranges them.
// A tag covers the node it sits on together with everything under it, which is
// why each mention is shown as that whole subtree, preceded by where it sits.
export function OutlineMentions({
  subject,
  tags,
}: {
  subject: OutlineSubject;
  tags: readonly DocumentMentionFragment[];
}) {
  const auth = useAuth();
  const [addState, add] = useMutation(AddOutlineNodeDocument);

  const mentions = React.useMemo(() => {
    const nodes = tags.map((tag) => tag.node).filter((node) => node !== null);
    const tagged = new Set(nodes.map((node) => node.id));

    // A node whose ancestor carries the same tag is already on screen inside it.
    return nodes
      .filter((node) => !node.ancestorsList?.some((ancestor) => tagged.has(ancestor.id)))
      .map((node) => ({
        id: node.id,
        ancestors: node.ancestorsList ?? [],
        rows: (node.subtreeList ?? []).map(nodeToRow),
      }));
  }, [tags]);

  if (!auth.isSystemAdmin) {
    return null;
  }

  // The tag rides inside the new node's content; the database projects it back
  // out, so nothing else has to be written to make the note show up here.
  const addNote = () =>
    add({
      input: {
        parent: null,
        content: JSON.stringify({
          type: 'bulletListItem',
          props: {},
          content: [
            {
              type: 'tag',
              props: { kind: subject.kind, refId: subject.id, label: subject.label },
            },
            { type: 'text', text: ' ', styles: {} },
          ],
        }),
      },
    });

  return (
    <div className="grid gap-4">
      <div className="flex items-center justify-between gap-3">
        <h3 className={typographyCls({ variant: 'section' })}>Poznámky</h3>
        <button
          type="button"
          className={buttonCls({ variant: 'outline', size: 'sm' })}
          disabled={addState.fetching}
          onClick={addNote}
        >
          Přidat poznámku
        </button>
      </div>

      {mentions.length === 0 && (
        <p className="text-sm text-neutral-11">Zatím žádné poznámky.</p>
      )}

      {mentions.map((mention) => (
        <article
          key={mention.id}
          className="rounded-lg border border-neutral-4 bg-neutral-2 p-3"
        >
          <OutlineBreadcrumb ancestors={mention.ancestors} className="mb-1 text-xs">
            <Link href={`/outline/${mention.id}`} className="underline">
              Otevřít
            </Link>
          </OutlineBreadcrumb>
          <OutlineEditor rows={mention.rows} editable={false} />
        </article>
      ))}
    </div>
  );
}
