import { atom, createStore, type PrimitiveAtom, useAtomValue } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import type { SessionClaims } from '@/lib/session-claims';
import deepEqual from 'fast-deep-equal';
import { defaultTenant, getTenant, TenantCatalogEntry } from '@/tenant/catalog';
import { deleteCookie, getCookie, setCookie } from 'cookies-next/client';

interface BaseAuthState {
  user: null | {
    id: string;
    uLogin: string | null;
    uEmail: string;
  };
  persons: PersonFragment[];
  couples: CoupleFragment[];
  personIds: string[];
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
    const tenant = getTenant(getCookie(key));
    return tenant ? String(tenant.id) : initialValue;
  },
  setItem(key: string, nextValue: string) {
    if (typeof window === 'undefined') return;

    const tenant = getTenant(nextValue);
    if (!tenant) return;

    const tenantId = String(tenant.id);
    if (String(getCookie(key)) === tenantId) return;

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
  (get) => get(baseTenantIdAtom),
  (_get, set, nextValue) => {
    const tenant = getTenant(nextValue);
    const tenantId = String(tenant?.id ?? defaultTenant.id);
    set(baseTenantIdAtom, tenantId);

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

export const authLoadingAtom = atom(true);

// Non-credential marker that a server-set httpOnly session cookie exists. The
// credential itself lives only in the cookie; this just lets client code (e.g.
// UserRefresher) know to fetch the current user. Set by the auth route-handler
// callers, cleared on logout.
const baseSessionPresentAtom: PrimitiveAtom<boolean> = atom(
  storage.getItem('session') === '1',
);
export const sessionPresentAtom = atom<boolean, [boolean], void>(
  (get) => get(baseSessionPresentAtom),
  (get, set, next) => {
    if (get(baseSessionPresentAtom) !== next) {
      set(baseSessionPresentAtom, next);
      storage.setItem('session', next ? '1' : null);
    }
  },
);

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

export const authAtom = atom<
  BaseAuthState,
  [SessionClaims | null, UserAuthFragment | null],
  void
>(
  (get) => get(baseUserAtom) || defaultAuthState,
  (get, set, claims, user) => {
    let nextValue = defaultAuthState;

    if (user && claims) {
      const persons =
        user.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) || [];

      const tenantId = String(get(tenantIdAtom));
      const isGuest = claims.guest_tenant_ids?.includes(tenantId) ?? false;
      const isMember =
        claims.member_tenant_ids?.includes(tenantId) ?? claims.is_member ?? false;
      const isTrainer =
        claims.trainer_tenant_ids?.includes(tenantId) ?? claims.is_trainer ?? false;
      const isAdmin =
        claims.admin_tenant_ids?.includes(tenantId) ?? claims.is_admin ?? false;
      const isSystemAdmin = claims.is_system_admin ?? false;

      nextValue = {
        user,
        persons,
        couples: persons.flatMap((x) => x.allCouplesList || []),
        personIds: persons.map((x) => x.id),
        isLoggedIn: !!user?.id,
        isGuest,
        isMember,
        isTrainer,
        isTrainerOrAdmin: isTrainer || isAdmin,
        isAdmin: isAdmin || isSystemAdmin,
        isSystemAdmin,
      };
    }

    // only update baseUserAtom if the derived state changes
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
