import { PaymentCategoryListDocument } from '@app/graphql/Payment';
import { TextField } from '@app/ui/fields/text';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { RenderListItem } from './generic/AdminEntityList';
import { buttonCls } from './style/button';
import { useAuth } from './use-auth';

export function PaymentCategoryList() {
  const router = useRouter();
  const { perms } = useAuth();

  const [{ data }] = useQuery({ query: PaymentCategoryListDocument });

  const nodes = React.useMemo(() => {
    return (data?.platbyCategories?.nodes || []).map((item) => ({
      id: item.id,
      title: item.pcName,
      href: `/platby/structure/category/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Platby</div>

        {perms.isAdmin && (
          <Link
            href="/platby/structure/category/add"
            className={buttonCls({
              size: 'sm',
              variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
            })}
          >
            Přidat platbu
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

const noop = () => {};
