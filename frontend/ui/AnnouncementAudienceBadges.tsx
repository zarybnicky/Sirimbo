import { AnnouncementAudienceRole } from '@/graphql';
import { CohortBasicFragment } from '@/graphql/Cohorts';
import { cn } from '@/ui/cn';
import { isTruthy } from './truthyFilter';
import { AnnouncementAudienceFragment } from '@/graphql/Announcement';

const ROLE_LABEL: Record<AnnouncementAudienceRole, string> = {
  MEMBER: 'Člen',
  TRAINER: 'Trenér',
  ADMINISTRATOR: 'Administrátor',
};

interface Props {
  audiences?: AnnouncementAudienceFragment[];
  cohorts?: (CohortBasicFragment | null | undefined)[] | null;
  roles?: (AnnouncementAudienceRole | null | undefined)[] | null;
  className?: string;
}

export function AnnouncementAudienceBadges({ audiences, className }: Props) {
  const derivedCohorts = audiences?.map((x) => x.cohort).filter(isTruthy) || [];
  const derivedRoles = audiences?.map((x) => x.audienceRole).filter(isTruthy) || [];

  const hasCohorts = derivedCohorts.length > 0;
  const hasRoles = derivedRoles.length > 0;

  return (
    <div
      className={cn(
        'flex items-center flex-wrap gap-2 text-xs text-neutral-11',
        className,
      )}
    >
      {hasCohorts ? (
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
      ) : null}

      {hasRoles ? (
        <div className="flex flex-wrap gap-1">
          {derivedRoles.map((role) => (
            <span
              key={role}
              className="inline-flex items-center rounded-full border border-neutral-7 bg-neutral-2 px-2 py-0.5 text-[11px] font-medium uppercase tracking-wide text-neutral-11"
            >
              {ROLE_LABEL[role]}
            </span>
          ))}
        </div>
      ) : null}

      {!hasCohorts && !hasRoles ? (
        <span className="inline-flex items-center rounded-full border border-dashed border-neutral-7 px-2 py-0.5 text-[11px] font-medium uppercase tracking-wide text-neutral-9">
          Viditelné pro všechny
        </span>
      ) : null}
    </div>
  );
}
