import { RenderListItem } from '@/ui/generic/AdminEntityList';
import React from 'react';
import { CohortGroupListDocument } from '@/graphql/CohortGroup';
import { useFuzzySearch } from './use-fuzzy-search';
import { useQuery } from 'urql';
import { buttonCls } from '@/ui/style';
import { TextField } from './fields/text';
import { Virtuoso } from 'react-virtuoso';
import Link from 'next/link';
import { Plus } from 'lucide-react';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterId,
});

export function CohortGroupList() {
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const [{ data, fetching }] = useQuery({ query: CohortGroupListDocument });

  const nodes = React.useMemo(() => {
    return (data?.cohortGroups?.nodes || []).map((x) => ({
      id: x.id,
      title: x.name,
      href: `/treninkove-programy/${x.id}`,
    }));
  }, [data]);
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Tréninkové programy</div>
        <Link
          href="/treninkove-programy/add"
          className={buttonCls({
            size: 'sm',
            variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
          })}
        >
          <Plus />
          Vytvořit
        </Link>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={RenderListItem}
        context={{ currentId, loading: fetching, loadMore: () => {} }}
      />
    </div>
  );
}
