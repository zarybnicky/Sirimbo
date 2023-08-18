import { RenderListItem } from '@app/ui/generic/AdminEntityList';
import React from 'react';
import { PaymentGroupListDocument } from '@app/graphql/Payment';
import { useRouter } from 'next/router';
import { useFuzzySearch } from './use-fuzzy-search';
import { useQuery } from 'urql';
import { buttonCls } from './style/button';
import { TextField } from './fields/text';
import { Virtuoso } from 'react-virtuoso';
import Link from 'next/link';
import { Plus } from 'lucide-react';

export function PaymentGroupList() {
  const [{ data, fetching }] = useQuery({ query: PaymentGroupListDocument });

  const nodes = React.useMemo(() => {
    return (data?.platbyGroups?.nodes || []).map((x) => ({
      id: x.id,
      title: x.pgName,
      href: `/platby/structure/group/${x.id}`,
    }));
  }, [data]);
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Skupiny plateb</div>
        <Link
          href="/platby/structure/group/add"
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
        context={{ router, loading: fetching, loadMore: () => {} }}
      />
    </div>
  );
}
