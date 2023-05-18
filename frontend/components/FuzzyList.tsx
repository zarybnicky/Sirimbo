import React from 'react';
import MiniSearch from 'minisearch';
import { Virtuoso } from 'react-virtuoso';

export const FuzzyList = <T extends { id: string | number }>({
  data,
  fields,
  search,
  renderItem,
}: {
  data: T[];
  fields: (keyof T)[];
  search?: string | undefined;
  renderItem: (index: number, item: T) => React.ReactNode;
}) => {
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

  const nodes = React.useMemo(() => {
    if (!search) {
      return data;
    }
    return index.search(search).map(({ id }) => nodesById[id]!);
  }, [data, nodesById, index, search]);

  return (
    <Virtuoso
      className="grow h-full overflow-y-auto scrollbar"
      data={nodes}
      itemContent={renderItem}
    />
  );
};
