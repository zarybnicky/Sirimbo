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
    folders: settings.evidenceFolders?.map<[string, string]>((x) =>
      typeof x === 'string' ? [x, '?'] : x,
    ) ?? [],
    seasons: settings.evidenceSeasons?.map<[string, string]>((x) =>
      typeof x === 'string' ? [x, '?'] : x,
    ) ?? [],
    courses: (
      settings.evidenceCourses?.map<[string, string, string]>((x) =>
        typeof x === 'string' ? [x, '?', '?'] : x,
      ) ?? []
    ).toSorted((x, y) => x[1].localeCompare(y[1])),
  };
});
