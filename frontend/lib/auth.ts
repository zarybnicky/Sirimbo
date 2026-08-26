import { atom, createStore, type PrimitiveAtom, useAtomValue } from 'jotai';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { SESSION_COOKIE, SESSION_PRESENT_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, TenantCatalogEntry } from '@/tenant/catalog';
import { deleteCookie, getCookie, setCookie } from 'cookies-next/client';

export type SessionClaims = {
  guest_tenant_ids: number[];
  member_tenant_ids: number[];
  trainer_tenant_ids: number[];
  admin_tenant_ids: number[];
  is_system_admin: boolean;
};

export type RequestAuthState = {
  claims: SessionClaims | null;
  user: UserAuthFragment | null;
};

export const authAtom = atom<RequestAuthState>({ claims: null, user: null });
export const tenantAtom = atom<TenantCatalogEntry>(defaultTenant);

export interface AuthState {
  user: null | {
    id: string;
    uLogin: string | null;
    uEmail: string;
  };
  persons: PersonFragment[];
  couples: CoupleFragment[];
  personIds: string[];
  tenantIds: number[];
  isExternal: boolean;
  isGuest: boolean;
  isMember: boolean;
  isTrainer: boolean;
  isAdmin: boolean;
  isSystemAdmin: boolean;
  isTrainerOrAdmin: boolean;
  isLoggedIn: boolean;
  isMyPerson: (id: string | null | undefined) => boolean;
  isMyCouple: (id: string | null | undefined) => boolean;
}

const defaultAuthState: AuthState = {
  user: null,
  persons: [],
  couples: [],
  personIds: [],
  tenantIds: [],
  isExternal: true,
  isGuest: false,
  isMember: false,
  isTrainer: false,
  isAdmin: false,
  isSystemAdmin: false,
  isTrainerOrAdmin: false,
  isLoggedIn: false,
  isMyPerson: () => false,
  isMyCouple: () => false,
};

export const storeRef = {
  current: createStore(),
  resetUrqlClient() {},
};

const storage = {
  getItem(key: string): string | null {
    return typeof localStorage === 'undefined' ? null : localStorage.getItem(key);
  },
  setItem(key: string, value: string | null) {
    if (value) {
      localStorage.setItem(key, value);
    } else {
      localStorage.removeItem(key);
    }
  },
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

// Keep this until browser-only legacy tokens no longer need time to become sessions.
const baseAuthLoadingAtom = atom(true);
export const authLoadingAtom = atom(
  (get) => !get(authAtom).user && get(baseAuthLoadingAtom),
  (_get, set, loading: boolean) => set(baseAuthLoadingAtom, loading),
);

export const sessionPresentAtom: PrimitiveAtom<boolean> = atom(
  getCookie(SESSION_PRESENT_COOKIE) === '1',
);
sessionPresentAtom.onMount = (setPresent) => {
  setPresent(getCookie(SESSION_PRESENT_COOKIE) === '1');
};

const baseTokenAtom: PrimitiveAtom<string | null> = atom(storage.getItem('token'));

export const tokenAtom = atom<string | null, [string | null], void>(
  (get) => get(baseTokenAtom),
  (get, set, nextValue) => {
    if (get(baseTokenAtom) !== nextValue) {
      set(baseTokenAtom, nextValue);
      storage.setItem('token', nextValue);
    }
  },
);

function resolveAuthState(
  claims: SessionClaims | null,
  user: UserAuthFragment | null,
  tenantId: number,
) {
  if (!user || !claims) return defaultAuthState;

  const persons = user.userProxiesList.flatMap((x) => (x.person ? [x.person] : []));
  const isGuest = claims.guest_tenant_ids.includes(tenantId);
  const isMember = claims.member_tenant_ids.includes(tenantId);
  const isTrainer = claims.trainer_tenant_ids.includes(tenantId);
  const isAdmin = claims.admin_tenant_ids.includes(tenantId);
  const isSystemAdmin = claims.is_system_admin;

  const tenantIds = new Set([
    ...claims.guest_tenant_ids,
    ...claims.member_tenant_ids,
    ...claims.trainer_tenant_ids,
    ...claims.admin_tenant_ids,
  ]);

  return {
    user,
    persons,
    couples: persons.flatMap((x) => x.allCouplesList || []),
    personIds: persons.map((x) => x.id),
    tenantIds: [...tenantIds],
    isLoggedIn: true,
    isExternal: persons.length === 0,
    isGuest,
    isMember,
    isTrainer,
    isTrainerOrAdmin: isTrainer || isAdmin,
    isAdmin: isAdmin || isSystemAdmin,
    isSystemAdmin,
  };
}

const authHelpersAtom = atom<AuthState>((get) => {
  const { claims, user } = get(authAtom);
  const auth = resolveAuthState(claims, user, get(tenantAtom).id);
  return {
    ...auth,
    isMyPerson: (id: string | null | undefined) => !!id && auth.personIds.includes(id),
    isMyCouple: (id: string | null | undefined) =>
      !!id && auth.couples.some((x) => x.id === id),
  };
});

export const useTenantId = () => useAtomValue(tenantIdAtom);
export const useTenantConfig = () => useAtomValue(tenantAtom).config;

export const useAuth = () => useAtomValue(authHelpersAtom);
export const useAuthLoading = () => useAtomValue(authLoadingAtom);

export function clearLegacySession() {
  if (typeof window !== 'undefined') {
    const { hostname } = window.location;
    deleteCookie(SESSION_COOKIE, { path: '/f' });
    if (!['localhost', '127.0.0.1', '::1'].includes(hostname)) {
      deleteCookie(SESSION_COOKIE, { path: '/f', domain: hostname });
    }
  }

  storeRef.current.set(tokenAtom, null);
}

export async function signOut() {
  const response = await fetch('/api/auth/logout', { method: 'POST' });
  if (!response.ok) throw new Error('Odhlášení selhalo');

  clearLegacySession();
  storeRef.current.set(authAtom, { claims: null, user: null });
  storeRef.current.set(sessionPresentAtom, false);
  storeRef.resetUrqlClient();
}
