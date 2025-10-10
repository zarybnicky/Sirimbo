import { AnnouncementItem } from '@/ui/AnnouncementItem';
import { Pagination } from '@/ui/Pagination';
import { cn } from '@/ui/cn';
import { MyAnnouncementsDocument, StickyAnnouncementsDocument } from '@/graphql/Announcement';
import { typographyCls } from '@/ui/style';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { Check, ListFilter } from 'lucide-react';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
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
          className={cn(
            'inline-flex size-8 items-center justify-center rounded-xl border border-neutral-6 bg-neutral-1 text-neutral-11 shadow-sm transition-colors',
            'hover:bg-neutral-2 focus:outline-none focus-visible:ring-2 focus-visible:ring-accent-7 focus-visible:ring-offset-1 focus-visible:ring-offset-neutral-1',
            'data-[state=open]:text-accent-12'
          )}
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
                sort === value ? 'opacity-100' : 'opacity-0'
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
        <h1 className={typographyCls({ variant: 'smallHeading', className: 'mt-0 grow' })}>
          Aktuality
        </h1>

        <ToggleGroupPrimitive.Root
          value={state}
          onValueChange={(value) => value && setState(value)}
          type="single"
        >
          {options.map(({ label, id }) => (
            <ToggleGroupPrimitive.Item
              key={`group-item-${id}-${label}`}
              value={id}
              className={cn(
                'group data-[state=on]:bg-neutral-5 bg-neutral-3 text-neutral-11',
                'border-y px-2 py-1 text-sm first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x',
                'border-neutral-5 data-[state=on]:border-neutral-8',
                'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-neutral-8',
              )}
            >
              {label}
            </ToggleGroupPrimitive.Item>
          ))}
        </ToggleGroupPrimitive.Root>
        <AnnouncementSortControls sort={sort} onChange={setSort} className="mb-3" />
      </div>


      <div className="space-y-2 rounded-lg">
        {(data?.myAnnouncements?.nodes || []).map((a) => (
          <AnnouncementItem key={a.id} item={a} />
        ))}
      </div>

      {!!data?.myAnnouncements?.totalCount && (
        <Pagination
          total={data.myAnnouncements.totalCount}
          limit={5}
          page={page}
          setPage={setPage}
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
          <AnnouncementItem key={a.id} item={a} onlyTitle />
        ))}
      </div>
    </div>
  );
}
