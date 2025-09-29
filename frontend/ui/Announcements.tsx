import { AnnouncementItem } from '@/ui/AnnouncementItem';
import { Pagination } from '@/ui/Pagination';
import { cn } from '@/ui/cn';
import { MyAnnouncementsDocument, StickyAnnouncementsDocument } from '@/graphql/Announcement';
import { buttonCls, typographyCls } from '@/ui/style';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import * as React from 'react';
import { useQuery } from 'urql';

const options = [
  { label: 'Aktuální', id: 'current' },
  { label: 'Archiv příspěvků', id: 'archive' },
];

type SortOption = 'created' | 'updated';

function AnnouncementSortControls({
  sort,
  onChange,
  className,
}: {
  sort: SortOption;
  onChange: (next: SortOption) => void;
  className?: string;
}) {
  return (
    <div className={cn('flex flex-wrap items-center gap-2', className)}>
      <span className="text-xs uppercase tracking-wide text-neutral-11">Seřadit podle:</span>
      <ToggleGroupPrimitive.Root
        type="single"
        value={sort}
        onValueChange={(value) => value && onChange(value as SortOption)}
        className="inline-flex rounded-xl shadow-md"
      >
        <ToggleGroupPrimitive.Item
          value="created"
          className={buttonCls({
            size: 'sm',
            variant: 'outline',
            className:
              'shadow-none rounded-none first:rounded-l-xl last:rounded-r-xl data-[state=on]:bg-accent-9 data-[state=on]:text-accent-0 data-[state=on]:border-accent-9',
          })}
        >
          Data vytvoření
        </ToggleGroupPrimitive.Item>
        <ToggleGroupPrimitive.Item
          value="updated"
          className={buttonCls({
            size: 'sm',
            variant: 'outline',
            className:
              'shadow-none rounded-none first:rounded-l-xl last:rounded-r-xl data-[state=on]:bg-accent-9 data-[state=on]:text-accent-0 data-[state=on]:border-accent-9',
          })}
        >
          Poslední úpravy
        </ToggleGroupPrimitive.Item>
      </ToggleGroupPrimitive.Root>
    </div>
  );
}

export function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [state, setState] = React.useState('current');
  const [sort, setSort] = React.useState<SortOption>('created');
  const orderByUpdated = sort === 'updated';

  const handleSortChange = React.useCallback((next: SortOption) => {
    setSort(next);
  }, []);

  const [{ data }] = useQuery({
    query: MyAnnouncementsDocument,
    variables: {
      first: 5,
      offset: (page - 1) * 5,
      archive: state === 'archive',
      orderByUpdated,
    },
  });

  React.useEffect(() => {
    setPage(1);
  }, [state, sort]);

  return (
    <div className="flex flex-col">
      <div className="mb-4 flex gap-6 flex-wrap grow-0 h-min justify-between items-baseline relative">
        <h1 className={typographyCls({ variant: 'smallHeading', className: 'mt-0' })}>
          Aktuality
        </h1>

        <ToggleGroupPrimitive.Root
          value={state}
          onValueChange={(value) => value && setState(value)}
          type="single"
          className="grow"
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
      </div>

      <AnnouncementSortControls sort={sort} onChange={handleSortChange} className="mb-3" />

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
  const [sort, setSort] = React.useState<SortOption>('created');
  const orderByUpdated = sort === 'updated';

  const handleSortChange = React.useCallback((next: SortOption) => {
    setSort(next);
  }, []);

  const [{ data }] = useQuery({
    query: StickyAnnouncementsDocument,
    variables: { orderByUpdated },
  });

  if (!data?.stickyAnnouncements) {
    return null;
  }

  return (
    <div className="flex flex-col">
      <div className="flex flex-wrap items-center justify-between gap-3 mb-5">
        <h4 className="text-2xl tracking-wide">Stálá nástěnka</h4>
        <AnnouncementSortControls sort={sort} onChange={handleSortChange} />
      </div>
      <div className="space-y-2 rounded-lg">
        {data.stickyAnnouncements.nodes.map((a) => (
          <AnnouncementItem key={a.id} item={a} onlyTitle />
        ))}
      </div>
    </div>
  );
}
