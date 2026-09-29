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
      onClick={isTitleOnly || expanded ? undefined : open}
      className={cardCls({ className: isTitleOnly || !expanded ? 'cursor-pointer' : '' })}
    >
      {isTitleOnly && (
        <Link
          href={`/nastenka/${item.id}`}
          aria-label={`Zobrazit příspěvek ${item.title}`}
          className="absolute inset-0 z-10 rounded-lg focus-visible:ring-3 focus-visible:ring-accent-7"
        />
      )}
      <div className="flex justify-between gap-2 items-start">
        <h3 className={typographyCls({ className: 'min-w-0', variant: 'cardHeading' })}>
          {isTitleOnly ? (
            item.title
          ) : expanded ? (
            <button type="button" className="cursor-pointer" onClick={() => setExpanded(false)}>
              {item.title}
            </button>
          ) : (
            <Link
              href={`/nastenka/${item.id}`}
              onClick={(event) => event.stopPropagation()}
            >
              {item.title}
            </Link>
          )}
        </h3>

        {actions && (
          <ActionGroup
            className={isTitleOnly ? 'relative z-20 ml-auto' : 'ml-auto'}
            actions={actions}
          />
        )}
      </div>

      <AnnouncementMeta item={item} />

      {!isTitleOnly &&
        (isEmpty ? (
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
        ))}
      {!isTitleOnly && (expanded || isEmpty) && (
        <FileAttachments attachments={item.explicitAttachments.nodes} />
      )}
    </div>
  );
}
