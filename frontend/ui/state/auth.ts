import { PrimitiveAtom, atom, createStore } from 'jotai';
import type { CoupleFragment } from '@/graphql/Memberships';
import type { PersonFragment } from '@/graphql/Person';
import type { UserAuthFragment } from '@/graphql/CurrentUser';
import deepEqual from 'fast-deep-equal';

export interface AuthState {
  user: null | {
    id: string;
    uLogin: string;
    uEmail: string;
  };
  persons: PersonFragment[];
  couples: CoupleFragment[];
  personIds: string[];
  isMember: boolean;
  isTrainer: boolean;
  isAdmin: boolean;
  isTrainerOrAdmin: boolean;
  isLoggedIn: boolean;
  isLoading?: boolean;
}

const defaultAuthState: AuthState = {
  user: null,
  persons: [],
  couples: [],
  personIds: [],
  isMember: false,
  isTrainer: false,
  isAdmin: false,
  isTrainerOrAdmin: false,
  isLoggedIn: false,
  isLoading: true,
};

export const storeRef = {
  current: createStore(),
  resetUrqlClient() {},
};

const storage = {
  getItem(key: string): string | null {
    return typeof localStorage !== 'undefined' ? localStorage.getItem(key) : null;
  },
  setItem(key: string, value: string | null) {
    if (value) {
      localStorage.setItem(key, value);
    } else {
      localStorage.removeItem(key);
    }
  }
};

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
      const base64 = base64Url?.replace("-", "+").replace("_", "/");
      const jwt = base64 ? JSON.parse(window.atob(base64)) : {};

      const persons = user.userProxiesList.flatMap(x => x.person ? [x.person] : []) || [];

      nextValue = {
        user,
        persons,
        couples: persons.flatMap(x => x.allCouplesList || []),
        personIds: persons.map(x => x.id),

        isLoggedIn: !!user?.id,
        isMember: jwt.is_member,
        isTrainer: jwt.is_trainer,
        isAdmin: jwt.is_admin,
        isTrainerOrAdmin: jwt.is_admin || jwt.is_trainer,
      };
    }

    set(tokenAtom, token);
    if (!deepEqual(nextValue, get(baseUserAtom))) {
      set(baseUserAtom, nextValue);
      storage.setItem('user', nextValue ? JSON.stringify(nextValue) : null);
    }
  },
);
