import { useSessionStorage } from '@/lib/use-local-storage';
import { TextField } from '@/ui/fields/text';
import { CohortForm } from '@/ui/forms/CohortForm';
import { ListItem } from '@/ui/ListItem';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useCohorts } from '@/ui/useCohorts';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { z } from 'zod';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';

const QueryParams = z.object({
  id: zRouterId,
});

export function CohortList() {
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const auth = useAuth();
  const [isArchive, setIsArchive] = useSessionStorage('cohortfilter-archive', undefined);

  const { data, fetching } = useCohorts({ visible: !isArchive });

  const nodes = React.useMemo(() => {
    return data.map((x) => ({
      id: x.id,
      title: x.name,
      subtitle: [!x.isVisible && 'Skrytá', x.location].filter(Boolean).join(', '),
      href: `/treninkove-skupiny/${x.id}`,
      children: (
        <div
          className="absolute rounded-l-xl w-4 shadow-sm inset-y-0 left-0"
          style={{ backgroundColor: x.colorRgb }}
        />
      ),
    }));
  }, [data]);
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Skupiny</div>

        {auth.isAdmin && (
          <Dialog>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <CohortForm />
            </DialogContent>
          </Dialog>
        )}

        {auth.isTrainerOrAdmin && (
          <div className="mt-2 w-full flex gap-2 justify-end">
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: isArchive ? 'primary' : 'outline' })}
              onClick={() => setIsArchive(x => x ? null : '1')}
            >
              Zobrazit archivované
            </button>
          </div>
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
        itemContent={ListItem}
        context={{ currentId, loading: fetching, loadMore: () => {} }}
      />
    </div>
  );
}
