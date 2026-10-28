import { AnnouncementCard } from '@/ui/AnnouncementCard';
import { Pagination } from '@/ui/Pagination';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { cn } from '@/lib/cn';
import { AnnouncementsDocument } from '@/graphql/Announcement';
import { buttonCls, typographyCls } from '@/ui/style';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { ArrowUpDown, Check } from 'lucide-react';
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
          <ArrowUpDown className="size-4" />
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
                'absolute left-2 size-4 text-accent-9 group-data-highlighted:text-accent-1 transition-opacity',
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

export const MyAnnouncements = React.memo(function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [state, setState] = React.useState('current');
  const [sort, setSort] = React.useState<SortOption>('updated');

  const [{ data }] = useQuery({
    query: AnnouncementsDocument,
    variables: {
      first: 5,
      offset: (page - 1) * 5,
      condition: { isSticky: false, status: state === 'archive' ? 'ARCHIVED' : 'PUBLISHED' },
      orderBy: [sort === 'updated' ? 'UPDATED_AT_DESC' : 'CREATED_AT_DESC', 'CREATED_AT_DESC'],
    },
  });

  return (
    <div className="flex flex-col">
      <div className="mb-4 flex gap-6 flex-wrap grow-0 h-min justify-between items-baseline relative">
        <h1
          className={typographyCls({ variant: 'smallHeading', className: 'mt-0 grow' })}
        >
          Aktuality
        </h1>

        <RadioButtonGroup
          name="announcement-state"
          value={state}
          options={options}
          onValueChange={(value) => {
            setState(value);
            setPage(1);
          }}
        />
        <AnnouncementSortControls
          sort={sort}
          onChange={(value) => {
            setSort(value);
            setPage(1);
          }}
          className="mb-3"
        />
      </div>

      <div className="space-y-2 rounded-lg">
        {(data?.announcements?.nodes || []).map((a) => (
          <AnnouncementCard key={a.id} item={a} />
        ))}
      </div>

      {!!data?.announcements?.totalCount && (
        <Pagination
          total={data.announcements.totalCount}
          limit={5}
          page={page}
          onPageChange={setPage}
        />
      )}
    </div>
  );
});

export const StickyAnnouncements = React.memo(function StickyAnnouncements() {
  const [sort, setSort] = React.useState<SortOption>('updated');

  const [{ data }] = useQuery({
    query: AnnouncementsDocument,
    variables: {
      condition: { isSticky: true, status: 'PUBLISHED' },
      orderBy: [sort === 'updated' ? 'UPDATED_AT_DESC' : 'CREATED_AT_DESC', 'CREATED_AT_DESC'],
    },
  });

  if (!data?.announcements) {
    return null;
  }

  return (
    <div className="flex flex-col">
      <div className="flex flex-wrap items-center justify-between gap-3 mb-5">
        <h4 className="text-2xl tracking-wide grow">Stálá nástěnka</h4>
        <AnnouncementSortControls sort={sort} onChange={setSort} />
      </div>
      <div className="space-y-2 rounded-lg">
        {data.announcements.nodes.map((a) => (
          <AnnouncementCard key={a.id} item={a} mode="titleOnly" />
        ))}
      </div>
    </div>
  );
});
