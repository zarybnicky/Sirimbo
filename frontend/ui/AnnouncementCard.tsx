import type { AnnouncementFragment } from '@/graphql/Announcement';
import { AnnouncementMeta } from '@/ui/AnnouncementShared';
import React from 'react';
import { cardCls, typographyCls } from './style';
import { RichTextView } from '@/ui/RichTextView';
import { ActionGroup } from './ActionGroup';
import { FileAttachments } from '@/ui/FileAttachments';
import { announcementActions } from '@/lib/actions/announcement';
import { useActions } from '@/lib/actions';
import Link from 'next/link';

interface Props {
  item: AnnouncementFragment;
  mode?: 'preview' | 'titleOnly';
}

export function AnnouncementCard({ item, mode = 'preview' }: Props) {
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);
  const actions = useActions(announcementActions, item);
  const isTitleOnly = mode === 'titleOnly';
  const isEmpty = !item.body.trim();

  return (
    <div
      onClick={expanded ? undefined : open}
      className={cardCls({ className: expanded ? '' : 'cursor-pointer' })}
    >
      <div className="flex justify-between gap-2 items-start">
        <h3 className={typographyCls({ className: 'min-w-0', variant: 'cardHeading' })}>
          {!isTitleOnly ? (
            <Link
              href={`/nastenka/${item.id}`}
              onClick={(event) => event.stopPropagation()}
            >
              {item.title}
            </Link>
          ) : expanded ? (
            <div className="cursor-pointer" onClick={() => setExpanded(false)}>
              {item.title}
            </div>
          ) : (
            item.title
          )}
        </h3>

        {actions && <ActionGroup className="ml-auto" actions={actions} />}
      </div>

      <AnnouncementMeta item={item} />

      {isTitleOnly || isEmpty ? (
        expanded ? (
          <RichTextView value={item.body} />
        ) : null
      ) : (
        <>
          <div className="relative pt-1">
            <RichTextView className={expanded ? '' : 'clamp-fade'} value={item.body} />
          </div>
          {!expanded && (
            <div className="absolute bottom-2 text-accent-11 font-bold">
              Zobrazit více...
            </div>
          )}
        </>
      )}
      {(expanded || (!isTitleOnly && isEmpty)) && (
        <FileAttachments attachments={item.explicitAttachments.nodes} />
      )}
    </div>
  );
}
