import { CoupleListDocument } from '@/graphql/Memberships';
import { TextField } from '@/ui/fields/text';
import { formatLongCoupleName } from '@/ui/format';
import { CreateCoupleButton } from '@/ui/forms/CreateCoupleForm';
import { RenderListItem } from '@/ui/generic/AdminEntityList';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export function CoupleList() {
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;

  const [{ data }] = useQuery({ query: CoupleListDocument });

  const nodes = React.useMemo(() => {
    return (data?.tenant?.couplesList || []).map((item) => ({
      id: item.id,
      title: formatLongCoupleName(item),
      href: `/pary/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Páry</div>
        <CreateCoupleButton />

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
        context={{ currentId, loadMore: noop, loading: false }}
      />
    </div>
  );
}

const noop = () => {}
