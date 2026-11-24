import { atom, createStore, useAtomValue } from 'jotai';
import { resolveAuth, type JwtClaims, type ResolvedAuth } from '@/lib/auth-claims';
import { logoutAction } from '@/lib/auth-actions';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { defaultTenant, getTenant, TenantCatalogEntry } from '@/tenant/catalog';
import { getCookie, setCookie } from 'cookies-next/client';

export type RequestAuthState = {
  claims: JwtClaims | null;
  user: UserAuthFragment | null;
};

export const authAtom = atom<RequestAuthState>({ claims: null, user: null });
export const tenantAtom = atom<TenantCatalogEntry>(defaultTenant);

export type AccessRequirements = {
  requireUser?: boolean;
  requireMember?: boolean;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  requireSystemAdmin?: boolean;
  requirePublicSite?: boolean;
  requireStarletImport?: boolean;
};

export interface AuthState extends ResolvedAuth {
  user: Pick<UserAuthFragment, 'id' | 'uLogin' | 'uEmail' | 'pendingApplications'> | null;
  persons: NonNullable<UserAuthFragment['userProxiesList'][number]['person']>[];
  couples: CoupleFragment[];
  isMyPerson: (id: string | null | undefined) => boolean;
  isMyCouple: (id: string | null | undefined) => boolean;
  canAccess: (requirements: AccessRequirements) => boolean;
  canManagePerson: (person: { tenantIds?: readonly (string | null)[] | null }) => boolean;
  canManageEvent: (event: { managerPersonIds: readonly (string | null)[] }) => boolean;
}

export const storeRef = {
  current: createStore(),
  resetUrqlClient() {},
};

export const tenantIdAtom = atom<string, [string], void>(
  (get) => get(tenantAtom).id.toString(),
  (_get, set, nextValue) => {
    const tenant = getTenant(nextValue) ?? defaultTenant;
    const tenantId = tenant.id.toString();
    set(tenantAtom, tenant);

    if (typeof window === 'undefined') return;

    if (getCookie('tenant_id') !== tenantId) {
      const { hostname, protocol } = window.location;
      setCookie('tenant_id', tenantId, {
        path: '/',
        domain:
          hostname === 'localhost' || hostname === '127.0.0.1'
            ? undefined
            : hostname.replace(/^www\./, ''),
        sameSite: 'lax',
        secure: protocol === 'https:',
        expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365 * 10),
      });
    }

    document.documentElement.dataset.tenant = tenantId;
  },
);

const signedInAuthAtom = atom((get) => {
  const session = get(authAtom);
  return session.claims && session.user ? session : null;
});

const authHelpersAtom = atom<AuthState>((get) => {
  const session = get(signedInAuthAtom);
  const tenant = get(tenantAtom);
  const user = session?.user ?? null;
  const auth = resolveAuth(session?.claims, tenant.id.toString());
  const isMember = auth.isMember || auth.isTrainer;
  const persons = user?.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) ?? [];
  return {
    ...auth,
    isMember,
    user,
    persons,
    couples: persons.flatMap((x) => x.couplesList || []),
    isMyPerson: (id: string | null | undefined) => !!id && auth.personIds.includes(id),
    isMyCouple: (id: string | null | undefined) => !!id && auth.coupleIds.includes(id),
    canAccess: (requirements) =>
      (!requirements.requireUser || auth.isLoggedIn) &&
      (!requirements.requireMember || isMember) &&
      (!requirements.requireTrainer || auth.isTrainer) &&
      (!requirements.requireAdmin || auth.isAdmin) &&
      (!requirements.requireSystemAdmin || auth.isSystemAdmin) &&
      (!requirements.requirePublicSite || !!tenant.config.publicSite) &&
      (!requirements.requireStarletImport || !!tenant.config.enableStarletImport),
    canManagePerson: (person) =>
      auth.isSystemAdmin || (auth.isAdmin && !!person.tenantIds?.some(
        (id) => id !== null && auth.adminTenantIds.includes(id),
      )),
    canManageEvent: (event) =>
      auth.isAdmin ||
      (auth.isTrainer &&
        auth.personIds.some((id) => event.managerPersonIds.includes(id))),
  };
});

export const useTenantId = () => useAtomValue(tenantIdAtom);
export const useTenantConfig = () => useAtomValue(tenantAtom).config;

export const useAuth = () => useAtomValue(authHelpersAtom);

export async function signOut() {
  await logoutAction();

  storeRef.current.set(authAtom, { claims: null, user: null });
  storeRef.resetUrqlClient();
}
