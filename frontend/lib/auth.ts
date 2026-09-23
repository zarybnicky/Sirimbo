import { atom, createStore, type PrimitiveAtom, useAtomValue } from 'jotai';
import {
  resolveAuth,
  type JwtClaims,
  type ResolvedAuth,
} from '@/lib/auth-claims';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { SESSION_COOKIE, SESSION_PRESENT_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, TenantCatalogEntry } from '@/tenant/catalog';
import { deleteCookie, getCookie, setCookie } from 'cookies-next/client';

export type RequestAuthState = {
  claims: JwtClaims | null;
  user: UserAuthFragment | null;
};

export const authAtom = atom<RequestAuthState>({ claims: null, user: null });
export const tenantAtom = atom<TenantCatalogEntry>(defaultTenant);

export interface AuthState extends ResolvedAuth {
  user: null | {
    id: string;
    uLogin: string | null;
    uEmail: string;
  };
  persons: PersonFragment[];
  couples: CoupleFragment[];
  isMyPerson: (id: string | null | undefined) => boolean;
  isMyCouple: (id: string | null | undefined) => boolean;
}

const defaultAuthState: AuthState = {
  ...resolveAuth(null, defaultTenant.id.toString()),
  user: null,
  persons: [],
  couples: [],
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

const authHelpersAtom = atom<AuthState>((get) => {
  const { claims, user } = get(authAtom);
  if (!user || !claims) return defaultAuthState;
  const auth = resolveAuth(claims, get(tenantIdAtom));
  const persons = user.userProxiesList.flatMap((x) => (x.person ? [x.person] : []));
  return {
    ...auth,
    user,
    persons,
    couples: persons.flatMap((x) => x.allCouplesList || []),
    isMyPerson: (id: string | null | undefined) => !!id && auth.personIds.includes(id),
    isMyCouple: (id: string | null | undefined) => !!id && auth.coupleIds.includes(id),
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
