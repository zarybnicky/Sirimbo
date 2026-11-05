import { UserRole } from '@/starlet/graphql';
import { atom } from 'jotai';
import { fetchStarlet } from '@/starlet/query';

type LoginToken =
  | { auth_ok: false }
  | {
      auth_ok: true;
      login: string;
      auth_token: string;
      role: UserRole;
      name: string;
      cgroup_key: string;
    };

const baseStarletTokenAtom = atom<LoginToken | undefined>();

export const starletTokenAtom = atom(
  (get) => get(baseStarletTokenAtom),
  (get, set, login?: string, password?: string) => {
    const prevToken = get(baseStarletTokenAtom);
    if (prevToken?.auth_ok && prevToken.login !== login) {
      return;
    }
    if (!login || !password) {
      set(baseStarletTokenAtom, undefined);
      return;
    }
    fetchStarlet('login' as any, { login, password }).then((x) => {
      set(baseStarletTokenAtom, x as LoginToken);
    });
  },
);

export type StarletCourse = { id: string; code: string; name: string };

export const starletRawSettingsAtom = atom<{
  evidenceAuth?: { login: string; password: string };
  evidenceFolders?: (string | [string, string])[];
  evidenceSeasons?: (string | [string, string])[];
  evidenceCourses?: (string | [string, string, string])[];
}>({});

export const starletSettingsAtom = atom((get) => {
  const settings = get(starletRawSettingsAtom);
  return {
    auth: settings.evidenceAuth,
    folders: settings.evidenceFolders?.map((x) =>
      typeof x === 'string' ? { id: x, name: '?' } : { id: x[0], name: x[1] },
    ) ?? [],
    seasons: settings.evidenceSeasons?.map((x) =>
      typeof x === 'string' ? { id: x, name: '?' } : { id: x[0], name: x[1] },
    ) ?? [],
    courses: (
      settings.evidenceCourses?.map<StarletCourse>((x) =>
        typeof x === 'string'
          ? { id: x, code: '?', name: '?' }
          : { id: x[0], code: x[1], name: x[2] },
      ) ?? []
    ).toSorted((x, y) => x.code.localeCompare(y.code)),
  };
});
