import type { TenantConfig } from '@/tenant/types';

export type JwtClaims = {
  user_id: string;
  tenant_id: string;
  email: string;
  my_person_ids: string[];
  my_tenant_ids: string[];
  my_cohort_ids: string[];
  my_couple_ids: string[];
  is_system_admin: boolean;
  guest_tenant_ids: string[];
  member_tenant_ids: string[];
  trainer_tenant_ids: string[];
  admin_tenant_ids: string[];
};

export function parseCurrentClaims(value: unknown): JwtClaims | null {
  if (!value) return null;

  const json = typeof value === 'string' ? value : JSON.stringify(value);
  return JSON.parse(json, (_key, claim) =>
    typeof claim === 'number' ? String(claim) : claim,
  ) as JwtClaims;
}

export function resolveAuth(claims: JwtClaims | null | undefined, tenantId: string) {
  const isSystemAdmin = claims?.is_system_admin ?? false;
  const isAdmin =
    isSystemAdmin || (claims?.admin_tenant_ids?.includes(tenantId) ?? false);
  const isTrainer =
    isAdmin || (claims?.trainer_tenant_ids?.includes(tenantId) ?? false);
  const isMember = claims?.member_tenant_ids?.includes(tenantId) ?? false;
  const isGuest = claims?.guest_tenant_ids?.includes(tenantId) ?? false;

  const role = isSystemAdmin
    ? 'system_admin'
    : isAdmin
      ? 'administrator'
      : isTrainer
        ? 'trainer'
        : isMember
          ? 'member'
          : 'anonymous';

  return {
    role,
    email: claims?.email,
    userId: claims?.user_id,
    personIds: claims?.my_person_ids ?? [],
    tenantIds: claims?.my_tenant_ids?.map(Number) ?? [],
    cohortIds: claims?.my_cohort_ids ?? [],
    coupleIds: claims?.my_couple_ids ?? [],
    isExternal: !claims?.my_person_ids.length,
    isGuest,
    isMember,
    isTrainer,
    isAdmin,
    isSystemAdmin,
    isLoggedIn: !!claims,
  };
}

export type ResolvedAuth = ReturnType<typeof resolveAuth>;

export type AccessRequirements = {
  requireUser?: boolean;
  requireMember?: boolean;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  requireSystemAdmin?: boolean;
  requirePublicSite?: boolean;
  requireStarletImport?: boolean;
};

export function canAccess(
  auth: ResolvedAuth,
  tenant: TenantConfig,
  x: AccessRequirements,
) {
  return (
    (!x.requireUser || auth.isLoggedIn) &&
    (!x.requireMember || auth.isMember || auth.isTrainer) &&
    (!x.requireTrainer || auth.isTrainer) &&
    (!x.requireAdmin || auth.isAdmin) &&
    (!x.requireSystemAdmin || auth.isSystemAdmin) &&
    (!x.requirePublicSite || !!tenant.publicSite) &&
    (!x.requireStarletImport || !!tenant.enableStarletImport)
  );
}
