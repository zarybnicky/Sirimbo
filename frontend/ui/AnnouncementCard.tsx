import type { AnnouncementFragment } from '@/graphql/Announcement';
import { AnnouncementMeta, useAnnouncementActions } from '@/ui/AnnouncementShared';
import { TitleActionsRow } from '@/ui/TitleBar';
import React from 'react';
import { cardCls } from './style';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import { RichTextView } from '@/ui/RichTextView';

interface Props {
  item: AnnouncementFragment;
  mode?: 'preview' | 'titleOnly';
}

export function AnnouncementCard({ item, mode = 'preview' }: Props) {
  const [expanded, setExpanded] = React.useState(false);
  const [editing, setEditing] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);
  const startEditing = React.useCallback(() => setEditing(true), []);
  const stopEditing = React.useCallback(() => setEditing(false), []);
  const actions = useAnnouncementActions(item, startEditing);
  const isTitleOnly = mode === 'titleOnly';

  if (editing) {
    return (
      <div className={cardCls()}>
        <AnnouncementForm id={item.id} data={item} onSuccess={stopEditing} />;
      </div>
    );
  }

  return (
    <div
      onClick={expanded ? undefined : open}
      className={cardCls({ className: expanded ? '' : 'cursor-pointer' })}
    >
      <TitleActionsRow
        title={
          expanded ? (
            <div className="cursor-pointer" onClick={() => setExpanded(false)}>
              {item.title}
            </div>
          ) : (
            item.title
          )
        }
        variant="cardHeading"
        actions={actions}
      />

      <AnnouncementMeta item={item} />

      {isTitleOnly ? (
        expanded ? (
          <RichTextView value={item.body} />
        ) : null
      ) : (
        <>
          <div className="relative pt-1">
            <RichTextView className={expanded ? '' : 'ClampFade'} value={item.body} />
          </div>
          {!expanded && (
            <div className="absolute bottom-2 text-accent-11 font-bold">
              Zobrazit více...
            </div>
          )}
        </>
      )}
    </div>
  );
}
