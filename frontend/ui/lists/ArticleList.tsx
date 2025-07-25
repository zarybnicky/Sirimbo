import { ArticlesDocument } from '@/graphql/Articles';
import { TextField } from '@/ui/fields/text';
import { fullDateFormatter } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { cn } from '@/ui/cn';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export function ArticleList() {
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const auth = useAuth();

  const [{ data }] = useQuery({ query: ArticlesDocument });

  const nodes = React.useMemo(() => {
    return (data?.aktualities?.nodes || []).map((item) => ({
      id: item.id,
      title: item.atJmeno,
      subtitle: item.atTimestampAdd ? fullDateFormatter.format(new Date(item.atTimestampAdd)) : '',
      href: {
        pathname: '/aktuality/[id]',
        query: { id: item.id },
      },
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Články</div>

        {auth.isAdmin && (
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

      <div className="grow h-full overflow-y-auto scrollbar">
        {fuzzy.map((item, index) => (
          <Link
            key={item.id}
            href={item.href}
            className={buttonCls({ variant: currentId === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
          >
            <div>{item.title}</div>
            <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
              {item.subtitle}
            </div>
          </Link>
        ))}
      </div>
    </div>
  );
}
