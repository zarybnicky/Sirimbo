// What is folded is remembered per browser, but nothing here is pruned by what
// happens to be on screen: a zoomed load only ever knows its own subtree, so
// dropping ids it cannot see would wipe every fold outside the zoom. The list is
// bounded instead, losing whatever was folded longest ago, and an id whose node
// no longer exists simply never matches again.
export const FOLD_LIMIT = 500;

export function fold(folded: readonly string[], id: string): string[] {
  return [id, ...folded.filter((seen) => seen !== id)].slice(0, FOLD_LIMIT);
}

export function unfold(folded: readonly string[], id: string): string[] {
  return folded.filter((seen) => seen !== id);
}

export type FoldStore = {
  isFolded: (id: string) => boolean;
  setFolded: (id: string, folded: boolean) => void;
};

// Storage can be missing or throw — a private window, blocked site data — and
// folding is a convenience, so it degrades to this session only.
export function foldStore(key = 'outline-folded'): FoldStore {
  let folded = read();

  function read(): string[] {
    try {
      const stored: unknown = JSON.parse(globalThis.localStorage?.getItem(key) ?? '[]');
      return Array.isArray(stored) ? stored.filter((id) => typeof id === 'string') : [];
    } catch {
      return [];
    }
  }

  return {
    isFolded: (id) => folded.includes(id),
    setFolded: (id, next) => {
      folded = next ? fold(folded, id) : unfold(folded, id);
      try {
        globalThis.localStorage?.setItem(key, JSON.stringify(folded));
      } catch {
        // Kept for this session only.
      }
    },
  };
}
