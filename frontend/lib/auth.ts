import { atom, createStore, useAtomValue } from 'jotai';
import { type JwtClaims, resolveAuth, type ResolvedAuth } from '@/lib/auth-claims';
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

export interface AuthState extends ResolvedAuth {
  user: Pick<UserAuthFragment, 'id' | 'uLogin' | 'uEmail' | 'pendingApplications'> | null;
  persons: NonNullable<UserAuthFragment['userProxiesList'][number]['person']>[];
  couples: CoupleFragment[];
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
  const auth = resolveAuth(session?.claims, tenant);
  const persons =
    user?.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) ?? [];
  return {
    ...auth,
    user,
    persons,
    couples: persons.flatMap((x) => x.couplesList || []),
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
