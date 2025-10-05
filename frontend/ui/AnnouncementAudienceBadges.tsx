import { cn } from '@/ui/cn';

export type AnnouncementAudienceRole = 'member' | 'trainer' | 'administrator';

export type AnnouncementAudienceCohort = {
  id: string;
  name?: string | null;
  colorRgb?: string | null;
};

type AnnouncementAudienceNodeLike = {
  id?: string | null;
  audienceRole?: AnnouncementAudienceRole | null;
  cohort?: AnnouncementAudienceCohort | null;
  cohortByUpsIdSkupina?: AnnouncementAudienceCohort | null;
  cohortByCohortId?: AnnouncementAudienceCohort | null;
};

type AnnouncementAudienceConnectionLike = {
  nodes?: (AnnouncementAudienceNodeLike | null | undefined)[] | null;
} | null;

const ROLE_LABEL: Record<AnnouncementAudienceRole, string> = {
  member: 'Člen',
  trainer: 'Trenér',
  administrator: 'Administrátor',
};

export interface AnnouncementAudienceBadgesProps {
  audiences?: AnnouncementAudienceConnectionLike;
  cohorts?: (AnnouncementAudienceCohort | null | undefined)[] | null;
  roles?: (AnnouncementAudienceRole | null | undefined)[] | null;
  className?: string;
}

export function AnnouncementAudienceBadges({
  audiences,
  cohorts,
  roles,
  className,
}: AnnouncementAudienceBadgesProps) {
  const derivedCohorts = collectCohorts(audiences, cohorts);
  const derivedRoles = collectRoles(audiences, roles);

  const hasCohorts = derivedCohorts.length > 0;
  const hasRoles = derivedRoles.length > 0;

  return (
    <div className={cn('flex items-center flex-wrap gap-2 text-xs text-neutral-11', className)}>
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

function collectCohorts(
  audiences?: AnnouncementAudienceConnectionLike,
  cohorts?: (AnnouncementAudienceCohort | null | undefined)[] | null,
) {
  const results: AnnouncementAudienceCohort[] = [];
  const seen = new Set<string>();

  const push = (cohort?: AnnouncementAudienceCohort | null) => {
    if (!cohort?.id || seen.has(cohort.id)) return;
    seen.add(cohort.id);
    results.push(cohort);
  };

  cohorts?.forEach((cohort) => push(cohort ?? undefined));

  audiences?.nodes?.forEach((node) => {
    if (!node) return;
    push(node.cohort ?? node.cohortByUpsIdSkupina ?? node.cohortByCohortId ?? undefined);
  });

  return results;
}

function collectRoles(
  audiences?: AnnouncementAudienceConnectionLike,
  roles?: (AnnouncementAudienceRole | null | undefined)[] | null,
) {
  const results: AnnouncementAudienceRole[] = [];
  const seen = new Set<AnnouncementAudienceRole>();

  const push = (role?: AnnouncementAudienceRole | null) => {
    if (!role || seen.has(role)) return;
    seen.add(role);
    results.push(role);
  };

  roles?.forEach((role) => push(role));

  audiences?.nodes?.forEach((node) => push(node?.audienceRole));

  return results;
}

