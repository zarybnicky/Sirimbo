import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { ArticlesDocument } from '@/graphql/Articles';
import { TextField } from '@/ui/fields/text';
import { buttonCls } from '@/ui/style';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import Link from 'next/link';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { z } from 'zod';
import { fullDateFormatter } from './format';
import { RenderListItem } from './generic/AdminEntityList';
import { useAuth } from './use-auth';

const QueryParams = z.object({
  id: zRouterId,
});

export function ArticleList() {
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const { perms } = useAuth();

  const [{ data }] = useQuery({ query: ArticlesDocument });

  const nodes = React.useMemo(() => {
    return (data?.aktualities?.nodes || []).map((item) => ({
      id: item.id,
      title: item.atJmeno,
      subtitle: item.atTimestampAdd ? fullDateFormatter.format(new Date(item.atTimestampAdd)) : '',
      href: `/aktuality/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Články</div>

        {perms.isAdmin && (
          <Link
            href="/aktuality/add"
            className={buttonCls({
              size: 'sm',
              variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
            })}
          >
            Vytvořit článek
          </Link>
        )}

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
