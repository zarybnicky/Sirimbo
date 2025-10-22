import { type PrimitiveAtom, atom, createStore } from 'jotai';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import deepEqual from 'fast-deep-equal';
import { tenantCatalog } from '@/tenant/catalog';
import type { TenantConfig } from '@/tenant/types';

interface AuthState {
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

const defaultAuthState: AuthState = {
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
  }
};

const emptyConfig: TenantConfig = {
  shortName: '',
  copyrightLine: '',
  favicon: '',
  enableHome: true,
  enableRegistration: true,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
};
const baseTenantIdAtom = atom<string>('');
export const tenantConfigAtom = atom<TenantConfig>(emptyConfig);
export const tenantIdAtom = atom<string, [string], void>(
  (get) => get(baseTenantIdAtom),
  (_get, set, nextValue) => {
    set(baseTenantIdAtom, nextValue);
    set(tenantConfigAtom, tenantCatalog[Number.parseInt(nextValue)]?.config ?? emptyConfig);
  },
);

export const authLoadingAtom = atom(true);

const baseTokenAtom: PrimitiveAtom<string | null> = atom(storage.getItem('token'));

const baseUserAtom: PrimitiveAtom<AuthState> = atom((() => {
  const item = storage.getItem('user');
  return item ? { ...defaultAuthState, ...JSON.parse(item) } : defaultAuthState;
})());

export const tokenAtom = atom<string | null, [string | null], void>(
  (get) => get(baseTokenAtom),
  (get, set, nextValue) => {
    if (get(baseTokenAtom) !== nextValue) {
      set(baseTokenAtom, nextValue);
      storage.setItem('token', nextValue);
    }
  },
);

export const authAtom = atom<AuthState, [string | null, UserAuthFragment | null], void>(
  (get) => get(baseUserAtom) || defaultAuthState,
  (get, set, token, user) => {
    let nextValue = defaultAuthState;

    if (user && token) {
      const base64Url = token.split(".")[1];
      const base64 = base64Url?.replaceAll("-", "+").replaceAll("_", "/");
      const jwt = base64 ? JSON.parse(atob(base64)) : {};

      const persons = user.userProxiesList.flatMap(x => x.person ? [x.person] : []) || [];

      const isSystemAdmin = !!jwt.is_system_admin;
      const isAdmin = !!jwt.is_admin || isSystemAdmin;
      const isTrainer = !!jwt.is_trainer;

      nextValue = {
        user,
        persons,
        couples: persons.flatMap(x => x.allCouplesList || []),
        personIds: persons.map(x => x.id),

        isLoggedIn: !!user?.id,
        isMember: !!jwt.is_member,
        isTrainer,
        isAdmin,
        isSystemAdmin,
        isTrainerOrAdmin: isAdmin || isTrainer,
      };
    }

    set(tokenAtom, token);
    if (!deepEqual(nextValue, get(baseUserAtom))) {
      set(baseUserAtom, nextValue);
      storage.setItem('user', nextValue ? JSON.stringify(nextValue) : null);
    }
  },
);
