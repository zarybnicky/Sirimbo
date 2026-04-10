import { Pencil } from 'lucide-react';
import type { AnnouncementFragment } from '@/graphql/Announcement';
import { announcementActions } from '@/lib/actions/announcement';
import { type Action, useActions } from '@/lib/actions';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import React from 'react';

export function useAnnouncementActions(
  item: AnnouncementFragment | null | undefined,
  onEdit: () => void,
) {
  const actions = React.useMemo<Action<AnnouncementFragment>[]>(() => {
    return [
      {
        id: 'announcement.edit',
        label: 'Upravit',
        icon: Pencil,
        visible: ({ auth }) => auth.isAdmin,
        type: 'mutation',
        execute: async () => {
          onEdit();
        },
      },
      ...announcementActions,
    ];
  }, [onEdit]);

  return useActions(actions, item);
}

export function AnnouncementMeta({ item }: { item: AnnouncementFragment }) {
  const authorName = item.author
    ? [item.author?.uJmeno, item.author?.uPrijmeni].filter(Boolean).join(' ')
    : undefined;

  return (
    <>
      <div className="flex items-center gap-1 text-sm text-neutral-11">
        <time
          dateTime={item.createdAt}
          title={numericFullFormatter.format(new Date(item.createdAt))}
        >
          {numericDateWithYearFormatter.format(new Date(item.createdAt))}
        </time>
        {item.updatedAt !== null && (
          <>
            <span>-</span>
            <time
              dateTime={item.updatedAt}
              title={numericFullFormatter.format(new Date(item.updatedAt))}
            >
              Upraveno
            </time>
          </>
        )}
        {authorName && (
          <>
            <span>-</span>
            <span>{authorName}</span>
          </>
        )}
      </div>

      {item.announcementAudiences.nodes.length > 0 && (
        <div className="flex flex-wrap items-baseline gap-4 my-2 text-sm text-neutral-12">
          <AnnouncementAudienceBadges audiences={item.announcementAudiences.nodes} />
        </div>
      )}
    </>
  );
}
