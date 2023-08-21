import { useRouter } from 'next/router';
import Link from 'next/link';
import { useQuery } from 'urql';
import { ArticlesDocument } from '@app/graphql/Articles';
import React from 'react';
import { buttonCls } from './style/button';
import { useAuth } from './use-auth';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { TextField } from '@app/ui/fields/text';
import { fullDateFormatter } from './format-date';
import { RenderListItem } from './generic/AdminEntityList';
import { Virtuoso } from 'react-virtuoso';

export function ArticleList() {
  const router = useRouter();
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
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Páry</div>

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
        context={{ router, loadMore: noop, loading: false }}
      />
    </>
  );
}

const noop = () => {}
