import { atom, createStore, type PrimitiveAtom, useAtomValue } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { SESSION_COOKIE, SESSION_PRESENT_COOKIE } from '@/lib/session-cookies';
import deepEqual from 'fast-deep-equal';
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
  tenantId: number;
  claims: SessionClaims | null;
  user: UserAuthFragment | null;
};

export const requestAuthAtom = atom<RequestAuthState | null>(null);

interface BaseAuthState {
  user: null | {
    id: string;
    uLogin: string | null;
    uEmail: string;
  };
  persons: PersonFragment[];
  couples: CoupleFragment[];
  personIds: string[];
  isExternal: boolean;
  isGuest: boolean;
  isMember: boolean;
  isTrainer: boolean;
  isAdmin: boolean;
  isSystemAdmin: boolean;
  isTrainerOrAdmin: boolean;
  isLoggedIn: boolean;
}

export interface AuthState extends BaseAuthState {
  isMyPerson: (id: string) => boolean;
  isMyCouple: (id: string) => boolean;
}

const defaultAuthState: BaseAuthState = {
  user: null,
  persons: [],
  couples: [],
  personIds: [],
  isExternal: true,
  isGuest: false,
  isMember: false,
  isTrainer: false,
  isAdmin: false,
  isSystemAdmin: false,
  isTrainerOrAdmin: false,
  isLoggedIn: false,
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

const cookieStorage = {
  getItem(key: string, initialValue: string) {
    return getTenant(getCookie(key))?.id.toString() ?? initialValue;
  },
  setItem(key: string, nextValue: string) {
    if (typeof window === 'undefined') return;

    const tenant = getTenant(nextValue);
    if (!tenant) return;

    const tenantId = tenant.id.toString();
    if (getCookie(key) === tenantId) return;

    const { hostname, protocol } = window.location;
    setCookie(key, tenantId, {
      path: '/',
      domain:
        hostname === 'localhost' || hostname === '127.0.0.1'
          ? undefined
          : hostname.replace(/^www\./, ''),
      sameSite: 'lax',
      secure: protocol === 'https:',
      expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365 * 10),
    });
  },
  removeItem(key: string) {
    deleteCookie(key, { path: '/' });
  },
};

const baseTenantIdAtom = atomWithStorage(
  'tenant_id',
  defaultTenant.id.toString(),
  cookieStorage,
  {
    getOnInit: true,
  },
);

export const tenantIdAtom = atom<string, [string], void>(
  (get) => get(requestAuthAtom)?.tenantId.toString() ?? get(baseTenantIdAtom),
  (get, set, nextValue) => {
    const tenantId = getTenant(nextValue)?.id ?? defaultTenant.id;
    const requestAuth = get(requestAuthAtom);
    if (requestAuth) set(requestAuthAtom, { ...requestAuth, tenantId });
    set(baseTenantIdAtom, tenantId.toString());

    if (typeof document === 'undefined') return;

    for (const cls of document.body.classList) {
      if (cls.includes('tenant-')) document.body.classList.remove(cls);
    }
    document.body.classList.add(`tenant-${tenantId}`);
  },
);
tenantIdAtom.onMount = (setTenantId) => {
  setTenantId(cookieStorage.getItem('tenant_id', String(defaultTenant.id)));
};
const tenantAtom = atom<TenantCatalogEntry>(
  (get) => getTenant(get(tenantIdAtom)) ?? defaultTenant,
);

export const useTenantId = () => useAtomValue(tenantIdAtom);
export const useTenantConfig = () => useAtomValue(tenantAtom).config;

const baseAuthLoadingAtom = atom(true);
export const authLoadingAtom = atom(
  (get) => get(requestAuthAtom) === null && get(baseAuthLoadingAtom),
  (_get, set, loading: boolean) => set(baseAuthLoadingAtom, loading),
);

export const sessionPresentAtom: PrimitiveAtom<boolean> = atom(
  getCookie(SESSION_PRESENT_COOKIE) === '1',
);
sessionPresentAtom.onMount = (setPresent) => {
  setPresent(getCookie(SESSION_PRESENT_COOKIE) === '1');
};

const baseTokenAtom: PrimitiveAtom<string | null> = atom(storage.getItem('token'));
const baseUserAtom: PrimitiveAtom<BaseAuthState> = atom(
  (() => {
    const item = storage.getItem('user');
    return item ? { ...defaultAuthState, ...JSON.parse(item) } : defaultAuthState;
  })(),
);

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

  return {
    user,
    persons,
    couples: persons.flatMap((x) => x.allCouplesList || []),
    personIds: persons.map((x) => x.id),
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

export const authAtom = atom<
  BaseAuthState,
  [SessionClaims | null, UserAuthFragment | null],
  void
>(
  (get) => {
    const requestAuth = get(requestAuthAtom);
    return requestAuth
      ? resolveAuthState(requestAuth.claims, requestAuth.user, requestAuth.tenantId)
      : get(baseUserAtom);
  },
  (get, set, claims, user) => {
    const tenantId = Number(get(tenantIdAtom));
    const nextValue = resolveAuthState(claims, user, tenantId);
    const requestAuth = get(requestAuthAtom);
    if (requestAuth) set(requestAuthAtom, { ...requestAuth, claims, user });

    // only update baseUserAtom if the claims-derived state changes
    if (!deepEqual(nextValue, get(baseUserAtom))) {
      set(baseUserAtom, nextValue);
      storage.setItem('user', nextValue ? JSON.stringify(nextValue) : null);
    }
  },
);

export const authHelpersAtom = atom((get) => {
  const auth = get(authAtom);
  return {
    ...auth,
    isMyPerson: (id: string | null | undefined) => !!id && auth.personIds.includes(id),
    isMyCouple: (id: string | null | undefined) =>
      !!id && auth.couples.some((x) => x.id === id),
  };
});

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
  storeRef.current.set(authAtom, null, null);
  storeRef.current.set(sessionPresentAtom, false);
  storeRef.resetUrqlClient();
}
