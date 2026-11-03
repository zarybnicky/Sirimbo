import { atom, createStore, type PrimitiveAtom, useAtomValue } from 'jotai';
import { resolveAuth, type JwtClaims, type ResolvedAuth } from '@/lib/auth-claims';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import { SESSION_PRESENT_COOKIE } from '@/lib/session-cookies';
import { defaultTenant, getTenant, TenantCatalogEntry } from '@/tenant/catalog';
import { getCookie, setCookie } from 'cookies-next/client';

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
  canManageEvent: (event: { managerPersonIds: readonly (string | null)[] }) => boolean;
}

const defaultAuthState: AuthState = {
  ...resolveAuth(null, defaultTenant.id.toString()),
  user: null,
  persons: [],
  couples: [],
  isMyPerson: () => false,
  isMyCouple: () => false,
  canManageEvent: () => false,
};

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

export const sessionPresentAtom: PrimitiveAtom<boolean> = atom(
  getCookie(SESSION_PRESENT_COOKIE) === '1',
);
sessionPresentAtom.onMount = (setPresent) => {
  setPresent(getCookie(SESSION_PRESENT_COOKIE) === '1');
};

const authHelpersAtom = atom<AuthState>((get) => {
  const { claims, user } = get(authAtom);
  if (!user || !claims) return defaultAuthState;
  const auth = resolveAuth(claims, get(tenantIdAtom));
  const persons = user.userProxiesList.flatMap((x) => (x.person ? [x.person] : []));
  return {
    ...auth,
    isMember: auth.isMember || auth.isTrainer || auth.isAdmin || auth.isSystemAdmin,
    user,
    persons,
    couples: persons.flatMap((x) => x.couplesList || []),
    isMyPerson: (id: string | null | undefined) => !!id && auth.personIds.includes(id),
    isMyCouple: (id: string | null | undefined) => !!id && auth.coupleIds.includes(id),
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
  const response = await fetch('/api/auth/logout', { method: 'POST' });
  if (!response.ok) throw new Error('Odhlášení selhalo');

  storeRef.current.set(authAtom, { claims: null, user: null });
  storeRef.current.set(sessionPresentAtom, false);
  storeRef.resetUrqlClient();
}
