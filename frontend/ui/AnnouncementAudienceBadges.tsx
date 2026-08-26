import { AnnouncementAudienceRole } from '@/graphql';
import { cn } from '@/lib/cn';
import { isTruthy } from '@/lib/truthyFilter';
import { AnnouncementAudienceFragment } from '@/graphql/Announcement';
import { badgeCls } from '@/ui/style';

const ROLE_LABEL: Record<AnnouncementAudienceRole, string> = {
  MEMBER: 'Člen',
  TRAINER: 'Trenér',
  ADMINISTRATOR: 'Administrátor',
};

interface Props {
  audiences?: AnnouncementAudienceFragment[];
  className?: string;
}

export function AnnouncementAudienceBadges({ audiences, className }: Readonly<Props>) {
  const derivedCohorts = audiences?.map((x) => x.cohort).filter(isTruthy) || [];
  const derivedRoles = audiences?.map((x) => x.audienceRole).filter(isTruthy) || [];

  return (
    <div
      className={cn(
        'flex items-center flex-wrap gap-2 text-xs text-neutral-11',
        className,
      )}
    >
      {derivedCohorts.length > 0 && (
        <div className="flex gap-0.5">
          {derivedCohorts.map((cohort) => (
            <div
              key={cohort.id}
              className="size-3 border border-neutral-6"
              title={cohort.name ?? undefined}
              style={{ backgroundColor: cohort.colorRgb ?? undefined }}
            />
          ))}
        </div>
      )}

      {derivedRoles.length > 0 && (
        <div className="flex flex-wrap gap-1">
          {derivedRoles.map((role) => (
            <span
              key={role}
              className={badgeCls()}
            >
              {ROLE_LABEL[role]}
            </span>
          ))}
        </div>
      )}
    </div>
  );
}
