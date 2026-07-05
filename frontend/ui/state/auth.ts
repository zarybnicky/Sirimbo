import { atom, createStore, type PrimitiveAtom } from 'jotai';
import { atomWithStorage } from 'jotai/utils';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import deepEqual from 'fast-deep-equal';
import { defaultTenant, parseTenant } from '@/tenant/catalog';
import type { TenantConfig } from '@/tenant/types';
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

const tenantCookieStorage = {
  getItem(key: string, initialValue: string) {
    const tenant = parseTenant(getCookie(key));
    return tenant ? String(tenant.id) : initialValue;
  },
  setItem(key: string, nextValue: string) {
    if (typeof window === 'undefined') return;

    const tenant = parseTenant(nextValue);
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
  String(defaultTenant.id),
  tenantCookieStorage,
  { getOnInit: true },
);

export const tenantIdAtom = atom<string, [string], void>(
  (get) => get(baseTenantIdAtom),
  (_get, set, nextValue) => {
    const tenantId = String((parseTenant(nextValue) ?? defaultTenant).id);
    set(baseTenantIdAtom, tenantId);

    if (typeof document === 'undefined') return;

    for (const cls of document.body.classList) {
      if (cls.includes('tenant-')) document.body.classList.remove(cls);
    }
    document.body.classList.add(`tenant-${tenantId}`);
  },
);
tenantIdAtom.onMount = (setTenantId) => {
  setTenantId(tenantCookieStorage.getItem('tenant_id', String(defaultTenant.id)));
};
export const tenantConfigAtom = atom<TenantConfig>(
  (get) => (parseTenant(get(tenantIdAtom)) ?? defaultTenant).config,
);

export const authLoadingAtom = atom(true);

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

      // For file uploads, only /f path but allow on subdomains
      if (typeof window !== 'undefined') {
        const { hostname } = window.location;
        const domain =
          hostname === 'localhost' || hostname === '127.0.0.1'
            ? undefined
            : hostname.replace(/^www\./, '');

        if (!nextValue) {
          deleteCookie('rozpisovnik', { path: '/f', domain });
        } else {
          setCookie('rozpisovnik', nextValue, {
            path: '/f',
            domain,
            sameSite: 'lax',
            secure: window.location.protocol === 'https:',
            expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365),
          });
        }
      }
    }
  },
);

export const authAtom = atom<
  BaseAuthState,
  [string | null, UserAuthFragment | null],
  void
>(
  (get) => get(baseUserAtom) || defaultAuthState,
  (get, set, token, user) => {
    let nextValue = defaultAuthState;

    if (user && token) {
      const base64Url = token.split('.')[1];
      const base64 = base64Url?.replaceAll('-', '+').replaceAll('_', '/');
      const jwt = base64 ? JSON.parse(atob(base64)) : {};

      const persons =
        user.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) || [];

      nextValue = {
        user,
        persons,
        couples: persons.flatMap((x) => x.allCouplesList || []),
        personIds: persons.map((x) => x.id),

        isLoggedIn: !!user?.id,
        isMember: !!jwt.is_member,
        isTrainer: !!jwt.is_trainer,
        isTrainerOrAdmin: !!jwt.is_trainer || !!jwt.is_admin || !!jwt.is_system_admin,
        isAdmin: !!jwt.is_admin || !!jwt.is_system_admin,
        isSystemAdmin: !!jwt.is_system_admin,
      };
    }

    set(tokenAtom, token);
    // only update baseUserAtom if the token payload changes
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
    isMyPerson: (id: string) => auth.personIds.includes(id),
    isMyCouple: (id: string) => auth.couples.some((x) => x.id === id),
  };
});
