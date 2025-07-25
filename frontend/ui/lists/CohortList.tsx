import { cn } from '@/ui/cn';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { CohortForm } from '@/ui/forms/CohortForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useCohorts } from '@/ui/useCohorts';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import Link from 'next/link';
import React from 'react';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export function CohortList() {
  const { query: { id: currentId } } = useTypedRouter(QueryParams);
  const auth = useAuth();
  const [isArchive, setIsArchive] = React.useState(false);

  const { data } = useCohorts({ visible: !isArchive });

  const nodes = React.useMemo(() => {
    return data.map((x) => ({
      id: x.id,
      title: x.name,
      subtitle: [!x.isVisible && 'Skrytá', x.location].filter(Boolean).join(', '),
      colorRgb: x.colorRgb,
      href: {
        pathname: '/treninkove-skupiny/[id]',
        query: { id: x.id },
      },
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
              onClick={() => setIsArchive(x => !x)}
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

      <div className="grow h-full overflow-y-auto scrollbar">
        {fuzzy.map((item) => (
          <Link
            key={item.id}
            href={item.href}
            className={buttonCls({ variant: currentId === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
          >
            <div>{item.title}</div>
            <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
              {item.subtitle}
            </div>
            <div
              className="absolute rounded-l-xl w-4 shadow-sm inset-y-0 left-0"
              style={{ backgroundColor: item.colorRgb }}
            />
          </Link>
        ))}
      </div>
    </div>
  );
}
