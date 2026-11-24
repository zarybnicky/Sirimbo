import { deleteCookie, getCookie, setCookie } from 'cookies-next/client';
import { atom } from 'jotai';
import { atomWithStorage, createJSONStorage } from 'jotai/utils';
import { z } from 'zod';
import type { EventSavedSearchFragment } from '@/graphql/Event';

export const UI_COOKIE = 'ui';

const SIDEBAR_MIN_WIDTH = 224;
const SIDEBAR_MAX_WIDTH = 448;

export function clampSidebarWidth(width: number) {
  return Math.round(Math.min(SIDEBAR_MAX_WIDTH, Math.max(SIDEBAR_MIN_WIDTH, width)));
}

const uiStateSchema = z.object({
  savedSearches: z
    .record(
      z.string(),
      z
        .custom<EventSavedSearchFragment>(
          (value) =>
            !!value &&
            typeof value === 'object' &&
            'id' in value &&
            typeof value.id === 'string' &&
            'condition' in value &&
            !!value.condition &&
            typeof value.condition === 'object',
        )
        .nullable(),
    )
    .optional(),
  sidebarWidth: z.number().int().min(SIDEBAR_MIN_WIDTH).max(SIDEBAR_MAX_WIDTH).nullable(),
});

type UiState = z.infer<typeof uiStateSchema>;

const defaultUiState: UiState = {
  sidebarWidth: null,
};

const storedUiStateSchema = uiStateSchema.catch(defaultUiState);

export function parseUiState(value?: string): UiState {
  try {
    return storedUiStateSchema.parse(JSON.parse(value ?? '{}'));
  } catch {
    return defaultUiState;
  }
}

const cookieStorage = createJSONStorage<UiState>(
  () => ({
    getItem: (key) => getCookie(key)?.toString() ?? null,
    setItem: (key, value) =>
      setCookie(key, value, {
        maxAge: 60 * 60 * 24 * 365,
        sameSite: 'lax',
        secure: typeof window !== 'undefined' && window.location.protocol === 'https:',
      }),
    removeItem: deleteCookie,
  }),
  {
    // JSON.parse calls the reviver with an empty key after constructing the root value.
    reviver: (key, value) => (key === '' ? storedUiStateSchema.parse(value) : value),
  },
);

export const uiAtom = atomWithStorage(UI_COOKIE, defaultUiState, cookieStorage);

export const sidebarWidthAtom = atom(
  (get) => get(uiAtom).sidebarWidth,
  (get, set, width: number) => {
    const sidebarWidth = clampSidebarWidth(width);
    set(uiAtom, { ...get(uiAtom), sidebarWidth });
  },
);
