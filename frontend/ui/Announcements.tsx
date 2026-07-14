import { AnnouncementCard } from '@/ui/AnnouncementCard';
import { Pagination } from '@/ui/Pagination';
import { cn } from '@/lib/cn';
import {
  MyAnnouncementsDocument,
  StickyAnnouncementsDocument,
} from '@/graphql/Announcement';
import { buttonCls, typographyCls } from '@/ui/style';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { Check, ListFilter } from 'lucide-react';
import * as React from 'react';
import { useQuery } from 'urql';

const options = [
  { label: 'Aktuální', id: 'current' },
  { label: 'Archiv příspěvků', id: 'archive' },
];

export type SortOption = 'created' | 'updated';

const sortOptions: Array<{ value: SortOption; label: string }> = [
  { value: 'updated', label: 'Poslední úpravy' },
  { value: 'created', label: 'Data vytvoření' },
];

export function AnnouncementSortControls({
  sort,
  onChange,
  className,
}: {
  sort: SortOption;
  onChange: (next: SortOption) => void;
  className?: string;
}) {
  return (
    <DropdownMenu>
      <div className={cn('inline-flex', className)}>
        <DropdownMenuTrigger
          aria-label="Změnit řazení oznámení"
          className={buttonCls({ variant: 'outline' })}
        >
          <ListFilter className="size-4" />
        </DropdownMenuTrigger>
      </div>
      <DropdownMenuContent align="end" className="w-48 p-1">
        {sortOptions.map(({ value, label }) => (
          <DropdownMenuButton
            key={value}
            onSelect={() => onChange(value)}
            className="relative flex w-full items-center gap-3 rounded-lg px-3 py-2 text-sm text-neutral-12 pl-8"
          >
            <Check
              className={cn(
                'absolute left-2 size-4 text-accent-9 group-data-[highlighted]:text-accent-1 transition-opacity',
                sort === value ? 'opacity-100' : 'opacity-0',
              )}
              aria-hidden
            />
            {label}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

export function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [state, setState] = React.useState('current');
  const [sort, setSort] = React.useState<SortOption>('updated');

  const [{ data }] = useQuery({
    query: MyAnnouncementsDocument,
    variables: {
      first: 5,
      offset: (page - 1) * 5,
      archive: state === 'archive',
      orderByUpdated: sort === 'updated',
    },
  });

  React.useEffect(() => {
    setPage(1);
  }, [state, sort]);

  return (
    <div className="flex flex-col">
      <div className="mb-4 flex gap-6 flex-wrap grow-0 h-min justify-between items-baseline relative">
        <h1
          className={typographyCls({ variant: 'smallHeading', className: 'mt-0 grow' })}
        >
          Aktuality
        </h1>

        <div className="inline-flex">
          {options.map(({ label, id }, index) => (
            <label key={id} className="relative">
              <input
                className="peer sr-only"
                type="radio"
                name="announcement-state"
                value={id}
                checked={state === id}
                onChange={() => setState(id)}
              />
              <span
                className={cn(
                  'block cursor-pointer bg-neutral-3 px-2 py-1 text-sm text-neutral-11',
                  'border-y border-l border-neutral-5',
                  'peer-checked:border-neutral-8 peer-checked:bg-neutral-5',
                  'peer-focus-visible:relative peer-focus-visible:z-30 peer-focus-visible:ring peer-focus-visible:ring-neutral-8',
                  index === 0 && 'rounded-l-xl',
                  index === options.length - 1 && 'rounded-r-xl border-r',
                )}
              >
                {label}
              </span>
            </label>
          ))}
        </div>
        <AnnouncementSortControls sort={sort} onChange={setSort} className="mb-3" />
      </div>

      <div className="space-y-2 rounded-lg">
        {(data?.myAnnouncements?.nodes || []).map((a) => (
          <AnnouncementCard key={a.id} item={a} />
        ))}
      </div>

      {!!data?.myAnnouncements?.totalCount && (
        <Pagination
          total={data.myAnnouncements.totalCount}
          limit={5}
          page={page}
          onPageChange={setPage}
        />
      )}
    </div>
  );
}

export function StickyAnnouncements() {
  const [sort, setSort] = React.useState<SortOption>('updated');

  const [{ data }] = useQuery({
    query: StickyAnnouncementsDocument,
    variables: {
      orderByUpdated: sort === 'updated',
    },
  });

  if (!data?.stickyAnnouncements) {
    return null;
  }

  return (
    <div className="flex flex-col">
      <div className="flex flex-wrap items-center justify-between gap-3 mb-5">
        <h4 className="text-2xl tracking-wide grow">Stálá nástěnka</h4>
        <AnnouncementSortControls sort={sort} onChange={setSort} />
      </div>
      <div className="space-y-2 rounded-lg">
        {data.stickyAnnouncements.nodes.map((a) => (
          <AnnouncementCard key={a.id} item={a} mode="titleOnly" />
        ))}
      </div>
    </div>
  );
}
