'use client';

import { AddOutlineNodeDocument, OutlineMentionsDocument } from '@/graphql/Document';
import { useAuth } from '@/lib/auth';
import { OutlineEditor } from '@/ui/fields/outline/OutlineEditor';
import { nodeText, nodeToRow } from '@/ui/fields/outline/blocks';
import { buttonCls, typographyCls } from '@/ui/style';
import Link from 'next/link';
import React from 'react';
import { useMutation, useQuery } from 'urql';

// The tag kinds that have a page to hang a mention list off. The editor names a
// tag by its kind; the query names the column that kind is projected into.
const SUBJECT_ARGUMENT = {
  person: 'person',
  couple: 'couple',
  cohort: 'cohort',
  event: 'eventInstance',
  series: 'eventSeries',
} as const;

export type OutlineSubject = {
  kind: keyof typeof SUBJECT_ARGUMENT;
  id: string;
  label: string;
};

// A tag covers the node it sits on together with everything under it, so each
// mention is shown as that whole subtree, preceded by where it sits in the
// outline. Read only: editing happens on the outline itself.
export function OutlineMentions({ subject }: { subject: OutlineSubject }) {
  const auth = useAuth();
  const [{ data }, refetch] = useQuery({
    query: OutlineMentionsDocument,
    variables: { [SUBJECT_ARGUMENT[subject.kind]]: subject.id },
    pause: !auth.isSystemAdmin,
  });
  const [addState, add] = useMutation(AddOutlineNodeDocument);

  // Stable across renders: the view only editor follows the rows it is handed,
  // so a fresh array each render would have it replace its content on every one.
  const mentions = React.useMemo(
    () =>
      (data?.documentMentionsList ?? []).map((mention) => ({
        id: mention.id,
        ancestors: mention.ancestorsList ?? [],
        rows: (mention.subtreeList ?? []).map(nodeToRow),
      })),
    [data],
  );

  if (!auth.isSystemAdmin) {
    return null;
  }

  // The new node carries the tag inline; the database projects it back out, so
  // the note appears here on the next read without anything else being written.
  const addNote = async () => {
    await add({
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
    refetch({ requestPolicy: 'network-only' });
  };

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
        <article key={mention.id} className="rounded-lg border border-neutral-4 bg-neutral-2 p-3">
          <nav className="mb-1 flex flex-wrap items-center gap-1 text-xs text-neutral-11">
            {mention.ancestors.map((ancestor) => (
              <React.Fragment key={ancestor.id}>
                <Link href={`/outline/${ancestor.id}`} className="underline">
                  {nodeText(ancestor.content) || 'Bez názvu'}
                </Link>
                <span aria-hidden>›</span>
              </React.Fragment>
            ))}
            <Link href={`/outline/${mention.id}`} className="underline">
              Otevřít
            </Link>
          </nav>
          <OutlineEditor rows={mention.rows} editable={false} />
        </article>
      ))}
    </div>
  );
}
