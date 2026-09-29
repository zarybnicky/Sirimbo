import { Paperclip } from 'lucide-react';
import type { AnnouncementFragment, AnnouncementStatus } from '@/graphql/Announcement';
import { AnnouncementAudienceBadges } from '@/ui/AnnouncementAudienceBadges';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { badgeCls } from '@/ui/style';

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

  return <span className={badgeCls({ variant: 'accent', className })}>{label}</span>;
}

export function AnnouncementMeta({ item }: { item: AnnouncementFragment }) {
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
        {item.authorName && (
          <>
            <span>-</span>
            <span>{item.authorName}</span>
          </>
        )}
        <AnnouncementStatusBadge status={item.status} />
        {item.explicitAttachments.nodes.length > 0 && (
          <>
            <span>-</span>
            <Paperclip aria-hidden className="size-3.5" />
            <span>{item.explicitAttachments.nodes.length}</span>
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
