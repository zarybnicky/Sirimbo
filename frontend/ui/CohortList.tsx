import { RenderListItem } from '@app/ui/generic/AdminEntityList';
import React from 'react';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { useRouter } from 'next/router';
import { useFuzzySearch } from './use-fuzzy-search';
import { useQuery } from 'urql';
import { buttonCls } from '@app/ui/style';
import { TextField } from './fields/text';
import { Virtuoso } from 'react-virtuoso';
import { Plus } from 'lucide-react';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { CohortForm } from './CohortForm';
import { useAuth } from './use-auth';

export function CohortList() {
  const { perms } = useAuth();
  const [{ data, fetching }] = useQuery({ query: CohortListDocument });

  const nodes = React.useMemo(() => {
    return (data?.skupinies?.nodes || []).filter(x => perms.isAdmin || x.sVisible).map((x) => ({
      id: x.id,
      title: x.sName,
      subtitle: [!x.sVisible && 'Skrytá', x.sLocation].filter(Boolean).join(', '),
      href: `/treninkove-skupiny/${x.id}`,
      children: (
        <div
          className="absolute rounded-l-lg w-4 shadow-sm inset-y-0 left-0"
          style={{ backgroundColor: x.sColorRgb }}
        />
      ),
    }));
  }, [data]);
  const router = useRouter();
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  const [addOpen, setAddOpen] = React.useState(false);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Skupiny</div>

        {perms.isAdmin && (
          <Dialog open={addOpen} onOpenChange={setAddOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({size: 'sm', variant: 'outline' })}>
                <Plus />
                Vytvořit
              </button>
            </DialogTrigger>
            <DialogContent>
              <CohortForm onSuccess={() => setAddOpen(false)} />
            </DialogContent>
          </Dialog>
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
        context={{ router, loading: fetching, loadMore: () => {} }}
      />
    </div>
  );
}
