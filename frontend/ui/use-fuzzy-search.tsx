import { rankItem } from '@tanstack/match-sorter-utils';
import React from 'react';

export function useFuzzySearch<T extends { id: string }>(
  data: T[],
  fields: (keyof T & string)[],
  search: string,
) {
  return React.useMemo(() => {
    const query = search.trim();
    if (!query) return data;

    const ranked: { item: T; rank: number; index: number }[] = [];
    for (const [index, item] of data.entries()) {
      let bestRank: number | null = null;
      for (const field of fields) {
        const result = rankItem(String(item[field] ?? ''), query);
        if (result.passed && (bestRank === null || result.rank > bestRank)) {
          bestRank = result.rank;
        }
      }
      if (bestRank !== null) {
        ranked.push({ item, rank: bestRank, index });
      }
    }

    return ranked
      .toSorted((a, b) => b.rank - a.rank || a.index - b.index)
      .slice(0, 50)
      .map(({ item }) => item);
  }, [fields, data, search]);
}
