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

const baseStarletRawSettingsAtom = atom<unknown>();
const baseStarletSettingsAtom = atom<{
  auth?: { login: string; password: string };
  folders: [string, string][];
  seasons: [string, string][];
  courses: [string, string, string][];
}>({
  folders: [],
  seasons: [],
  courses: [],
});

export const starletSettingsAtom = atom(
  (get) => get(baseStarletSettingsAtom),
  (get, set, raw: unknown) => {
    if (raw === get(baseStarletRawSettingsAtom)) return;
    set(baseStarletRawSettingsAtom, raw);
    set(baseStarletSettingsAtom, parseSettings(raw));
  },
);

function parseSettings(raw: unknown) {
  const settings = typeof raw === 'string' ? JSON.parse(raw) : raw;
  return {
    auth: settings?.['evidenceAuth'] as { login: string; password: string },
    folders: (settings?.['evidenceFolders']?.map((x: any) =>
      typeof x === 'string' ? [x, '?'] : x,
    ) || []) as [string, string][],
    seasons: (settings?.['evidenceSeasons']?.map((x: any) =>
      typeof x === 'string' ? [x, '?'] : x,
    ) || []) as [string, string][],
    courses: (
      (settings?.['evidenceCourses']?.map((x: any) =>
        typeof x === 'string' ? [x, '?', '?'] : x,
      ) || []) as [string, string, string][]
    ).toSorted((x, y) => x[1].localeCompare(y[1])),
  };
}
