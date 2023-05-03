import React from 'react';
import MiniSearch from 'minisearch';

export const FuzzyList = <T extends { id: any }>({
  data,
  fields,
  search,
  renderItem,
}: {
  data: T[];
  fields: (keyof T)[] & string[];
  search: string | undefined;
  renderItem: (item: T) => React.ReactNode;
}) => {
  const nodesById = React.useMemo(() => {
    return data.reduce((byId, document) => {
      byId[document.id] = document;
      return byId;
    }, {} as { [key: string]: T });
  }, [data]);

  const index = React.useMemo(() => {
    const index = new MiniSearch({
      fields: fields,
      searchOptions: {
        fuzzy: 0.2,
        prefix: true,
      },
    });
    index.addAll(data);
    return index;
  }, [fields, data]);

  const nodes = React.useMemo(() => {
    if (!search) {
      return data;
    }
    return index.search(search).map(({ id }) => nodesById[id]!);
  }, [data, nodesById, index, search]);

  return <>{nodes.map(renderItem)}</>;
};
