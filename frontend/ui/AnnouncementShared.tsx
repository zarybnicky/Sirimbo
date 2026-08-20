import { Pencil } from 'lucide-react';
import type { AnnouncementFragment, AnnouncementStatus } from '@/graphql/Announcement';
import {
  announcementActions,
  canManageAnnouncement,
} from '@/lib/actions/announcement';
import { type Action, useActions } from '@/lib/actions';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { cn } from '@/lib/cn';
import React from 'react';

const STATUS_LABEL: Partial<Record<AnnouncementStatus, string>> = {
  DRAFT: 'Koncept',
  SCHEDULED: 'Naplánováno',
  ARCHIVED: 'Archivováno',
};

export function AnnouncementStatusBadge({
  status,
  className,
}: {
  status: AnnouncementStatus;
  className?: string;
}) {
  const label = STATUS_LABEL[status];
  if (!label) return null;

  return (
    <span
      className={cn(
        'inline-flex rounded-full bg-accent-3 px-2 py-0.5 text-[11px] tracking-tight uppercase text-accent-11',
        className,
      )}
    >
      {label}
    </span>
  );
}

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
        visible: canManageAnnouncement,
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
        <AnnouncementStatusBadge status={item.status} />
      </div>

      {item.announcementAudiences.nodes.length > 0 && (
        <div className="flex flex-wrap items-baseline gap-4 my-2 text-sm text-neutral-12">
          <AnnouncementAudienceBadges audiences={item.announcementAudiences.nodes} />
        </div>
      )}
    </>
  );
}
