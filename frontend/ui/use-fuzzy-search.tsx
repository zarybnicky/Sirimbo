import React from 'react';
import fuzzysort from 'fuzzysort';

export function useFuzzySearch<T extends { id: string }>(
  data: T[],
  fields: (keyof T & string)[],
  search: string,
) {
  return React.useMemo(() => {
    return fuzzysort.go(search, data, {
      all: true,
      keys: fields,
      limit: search ? 50 : undefined,
      threshold: 0.5,
    }).map(x => x.obj);
  }, [fields, data, search]);
}
