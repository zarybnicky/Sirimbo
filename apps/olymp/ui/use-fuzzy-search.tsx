import React from 'react';
import MiniSearch from 'minisearch';

export function useFuzzySearch<T extends { id: string }>(
  data: T[],
  fields: (keyof T)[],
  search: string,
) {
  const nodesById = React.useMemo(() => {
    return data.reduce((byId, document) => {
      byId[document.id] = document;
      return byId;
    }, {} as { [key: string]: T });
  }, [data]);

  const index = React.useMemo(() => {
    const index = new MiniSearch({
      fields: fields as string[],
      searchOptions: {
        fuzzy: 0.2,
        prefix: true,
      },
    });
    index.addAll(data);
    return index;
  }, [fields, data]);

  return React.useMemo(() => {
    if (!search) {
      return data;
    }
    return index.search(search).map(({ id }) => nodesById[id]!);
  }, [data, nodesById, index, search]);
}
