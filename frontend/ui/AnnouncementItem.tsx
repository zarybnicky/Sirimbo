import type { AnnouncementFragment } from '@/graphql/Announcement';
import { CohortColorBoxes } from '@/ui/CohortColorBox';
import { RichTextView } from '@/ui/RichTextView';
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { fullDateFormatter } from '@/ui/format';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import React from 'react';
import { AnnouncementMenu } from './menus/AnnouncementMenu';
import { cardCls } from './style';

export function AnnouncementItem({
  item,
  onlyTitle,
}: {
  item: AnnouncementFragment;
  onlyTitle?: boolean;
}) {
  const [expanded, setExpanded] = React.useState(false);
  const [editing, setEditing] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);
  const close = React.useCallback(() => setExpanded(false), []);

  if (editing) {
    return (
      <div className={cardCls()}>
        <AnnouncementForm id={item.id} data={item} onSuccess={() => setEditing(false)} />
      </div>
    );
  }
  const expandedTitle = (
    <h2 className="text-lg font-bold mb-4 cursor-pointer" onKeyDown={close} onClick={close}>
      {item.upNadpis}
    </h2>
  );

  return (
    <div
      onClick={expanded ? undefined : open}
      className={cardCls({ className: expanded ? '' : 'cursor-pointer' })}
    >
      <AnnouncementMenu align="end" item={item} onEdit={() => setEditing(true)}>
        <DropdownMenuTrigger.CornerDots />
      </AnnouncementMenu>

      <div className="text-neutral-12 text-sm flex flex-wrap items-baseline gap-4">
        <div>
          {[
            fullDateFormatter.format(new Date(item.upTimestampAdd)),
            item.userByUpKdo &&
              `${item.userByUpKdo?.uJmeno} ${item.userByUpKdo?.uPrijmeni}`,
          ]
            .filter(Boolean)
            .join(', ')}
        </div>
        <CohortColorBoxes
          items={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
            (x) => x.cohortByUpsIdSkupina,
          )}
        />
      </div>

      {!onlyTitle ? (
        <div className="relative">
          {expanded ? expandedTitle : (
            <h2 className="text-lg font-bold mb-4">{item.upNadpis}</h2>
          )}
          <RichTextView className={expanded ? '' : 'ClampFade'} value={item.upText} />
          {!expanded && (
            <div className="absolute bottom-0 text-accent-11 font-bold">
              Zobrazit více...
            </div>
          )}
        </div>
      ) : expanded ? (
        <>
          {expandedTitle}
          <RichTextView value={item.upText} />
        </>
      ) : (
        <h2 className="text-lg font-bold">{item.upNadpis}</h2>
      )}
    </div>
  );
}
