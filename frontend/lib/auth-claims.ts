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
  member_tenant_ids: string[];
  trainer_tenant_ids: string[];
  admin_tenant_ids: string[];
};

export function normalizeClaims(
  value: Record<string, unknown> | null | undefined,
): JwtClaims | null {
  if (!value) return null;

  return Object.fromEntries(
    Object.entries(value)
      .filter(([key]) => !['exp', 'iat', 'aud', 'iss'].includes(key))
      .map(([key, claim]) => [
        key,
        Array.isArray(claim)
          ? claim.map(String)
          : typeof claim === 'number'
            ? String(claim)
            : claim,
      ]),
  ) as JwtClaims;
}

export type AccessRequirements = {
  requireLoggedIn?: boolean;
  requireMember?: boolean;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  requireSystemAdmin?: boolean;
  requirePublicSite?: boolean;
  requireStarletImport?: boolean;
};

export type AuthTenant = { id: number | string; config?: TenantConfig };

export function resolveAuth(claims: JwtClaims | null | undefined, tenant: AuthTenant) {
  const tenantId = String(tenant.id);
  const userId = claims?.user_id;
  const personIds = claims?.my_person_ids ?? [];
  const coupleIds = claims?.my_couple_ids ?? [];
  const adminTenantIds = claims?.admin_tenant_ids ?? [];
  const isLoggedIn = !!claims;
  const isSystemAdmin = claims?.is_system_admin ?? false;
  const isAdmin = isSystemAdmin || adminTenantIds.includes(tenantId);
  const isTrainer = isAdmin || (claims?.trainer_tenant_ids?.includes(tenantId) ?? false);
  const isMember = isTrainer || (claims?.member_tenant_ids?.includes(tenantId) ?? false);

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
    userId,
    currentTenantId: tenantId,
    allowedTenantIds: claims?.my_tenant_ids?.map(Number) ?? [],
    personIds,
    adminTenantIds,
    cohortIds: claims?.my_cohort_ids ?? [],
    coupleIds,
    isMember,
    isTrainer,
    isAdmin,
    isSystemAdmin,
    isLoggedIn,
    isMyPerson: (id: string | null | undefined) => !!id && personIds.includes(id),
    isMyCouple: (id: string | null | undefined) => !!id && coupleIds.includes(id),
    canAccess: (reqs: AccessRequirements) =>
      (!reqs.requireLoggedIn || isLoggedIn) &&
      (!reqs.requireMember || isMember) &&
      (!reqs.requireTrainer || isTrainer) &&
      (!reqs.requireAdmin || isAdmin) &&
      (!reqs.requireSystemAdmin || isSystemAdmin) &&
      (!reqs.requirePublicSite || !!tenant.config?.publicSite) &&
      (!reqs.requireStarletImport || !!tenant.config?.enableStarletImport),
    canManagePerson: (person?: { tenantIds: readonly string[] } | null) =>
      isSystemAdmin ||
      (isAdmin && !!person?.tenantIds.some((id) => adminTenantIds.includes(id))),
    canCreateEvent: isTrainer,
    canManageEvent: (event: { managerPersonIds: readonly string[] }) =>
      isAdmin ||
      (isTrainer && personIds.some((id) => event.managerPersonIds.includes(id))),
    canManageTenant: (x: { tenantId: string }) =>
      isSystemAdmin || adminTenantIds.includes(x.tenantId),
    canCreateAnnouncement: isTrainer,
    canManageAnnouncement: (x: { authorId: string | null }) =>
      isAdmin || (isTrainer && x.authorId === userId),
  };
}

export type ResolvedAuth = ReturnType<typeof resolveAuth>;
