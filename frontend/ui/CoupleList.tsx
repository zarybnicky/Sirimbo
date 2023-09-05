import { CreateCoupleButton } from '@/ui/CreateCoupleForm';
import { CoupleListDocument } from '@app/graphql/Memberships';
import { TextField } from '@app/ui/fields/text';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { useRouter } from 'next/router';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { formatLongCoupleName } from './format';
import { RenderListItem } from './generic/AdminEntityList';
import { useAuth } from './use-auth';

export function CoupleList() {
  const router = useRouter();
  const { perms } = useAuth();

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

        {perms.isAdmin && (
          <CreateCoupleButton />
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
    </div>
  );
}

const noop = () => {}
