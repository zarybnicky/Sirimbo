import type { AnnouncementFragment } from '@/graphql/Announcement';
import {
  AnnouncementAudienceBadges,
  type AnnouncementAudienceRole,
} from '@/ui/AnnouncementAudienceBadges';
import { RichTextView } from '@/ui/RichTextView';
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { AnnouncementForm } from '@/ui/forms/AnnouncementForm';
import React from 'react';
import { AnnouncementMenu } from './menus/AnnouncementMenu';
import { cardCls } from './style';

type MaybeAnnouncementAudience = {
  announcementAudiencesByAnnouncementId?: {
    nodes?: (MaybeAnnouncementAudienceNode | null | undefined)[] | null;
  } | null;
  announcementAudiences?: {
    nodes?: (MaybeAnnouncementAudienceNode | null | undefined)[] | null;
  } | null;
  audienceRoles?: (AnnouncementAudienceRole | null | undefined)[] | null;
};

type MaybeAnnouncementAudienceNode = {
  audienceRole?: AnnouncementAudienceRole | null;
  cohort?: MaybeAnnouncementCohort | null;
  cohortByUpsIdSkupina?: MaybeAnnouncementCohort | null;
  cohortByCohortId?: MaybeAnnouncementCohort | null;
};

type MaybeAnnouncementCohort = {
  id: string;
  name?: string | null;
  colorRgb?: string | null;
};

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

  const authorName = item.author
    ? [item.author?.uJmeno, item.author?.uPrijmeni].filter(Boolean).join(' ')
    : undefined;

  const expandedTitle = (
    <h2 className="text-lg font-bold mb-4 cursor-pointer" onKeyDown={close} onClick={close}>
      {item.title}
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

      <div className="flex items-center gap-1 text-neutral-11 text-xs -mt-1">
        <time dateTime={item.createdAt} title={numericFullFormatter.format(new Date(item.createdAt))}>
          {numericDateWithYearFormatter.format(new Date(item.createdAt))}
        </time>
        {item.updatedAt !== null && (
          <>
            <span>-</span>
            <time dateTime={item.updatedAt} title={numericFullFormatter.format(new Date(item.updatedAt))}>
              Upraveno
            </time>
          </>
        )}
      </div>

      <div className="relative text-neutral-12 text-sm flex flex-wrap items-baseline gap-4">
        {authorName && <div>{authorName}</div>}

        <AnnouncementAudienceBadges
          audiences={getAudienceConnection(item as unknown as MaybeAnnouncementAudience)}
          cohorts={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
            (x) => x.cohortByUpsIdSkupina,
          )}
          roles={(item as unknown as MaybeAnnouncementAudience).audienceRoles}
        />
      </div>

      {onlyTitle ? (expanded ? (
        <>
          {expandedTitle}
          <RichTextView value={item.body} />
        </>
      ) : (
        <h2 className="text-lg font-bold">{item.title}</h2>
      )) : (
        <>
          <div className="relative">
            {expanded ? expandedTitle : (
              <h2 className="text-lg font-bold mb-4">{item.title}</h2>
            )}
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

function getAudienceConnection(item: MaybeAnnouncementAudience) {
  return (
    item.announcementAudiencesByAnnouncementId ??
    item.announcementAudiences ??
    null
  );
}
