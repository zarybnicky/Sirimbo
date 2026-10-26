import { ListItem } from '@/ui/ListItem';
import { ArticlesDocument } from '@/graphql/Articles';
import { TextField } from '@/ui/fields/text';
import { fullDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import Link from 'next/link';
import { useParams, usePathname } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';

export function ArticleList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const pathname = usePathname() ?? '';
  const auth = useAuth();

  const [{ data }] = useQuery({ query: ArticlesDocument });
  const nodes = React.useMemo(() => data?.aktualities?.nodes || [], [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'atJmeno'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Články</div>

        {auth.isAdmin && (
          <Link
            href="/aktuality/add"
            className={buttonCls({
              size: 'sm',
              variant: pathname.endsWith('/add') ? 'primary' : 'outline',
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

      <div className="grow h-full overflow-y-auto scrollbar">
        {fuzzy.map((item) => (
          <ListItem
            key={item.id}
            href={`/aktuality/${item.id}`}
            title={item.atJmeno}
            selected={currentId === item.id}
            subtitle={
              <>
                {item.publishedAt
                  ? fullDateFormatter.format(new Date(item.publishedAt))
                  : 'Koncept'}
                {!item.isVisible && item.publishedAt ? ', skrytý' : ''}
              </>
            }
          />
        ))}
      </div>
    </div>
  );
}
