import { useRouter } from 'next/router';
import { useQuery } from 'urql';
import { CoupleListDocument } from '@app/graphql/Memberships';
import React from 'react';
import { NewCoupleDialog } from './NewCoupleDialog';
import { useAuth } from './use-auth';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { TextField } from '@app/ui/fields/text';
import { formatLongCoupleName } from './format';
import { Virtuoso } from 'react-virtuoso';
import { RenderListItem } from './generic/AdminEntityList';

export function CoupleList() {
  const router = useRouter();
  const { perms } = useAuth();

  const [{ data }] = useQuery({ query: CoupleListDocument });

  const nodes = React.useMemo(() => {
    return (data?.couples?.nodes || []).map((item) => ({
      id: item.id,
      title: formatLongCoupleName(item),
      href: `/pary/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Páry</div>

        {perms.isAdmin && (
          <NewCoupleDialog />
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
