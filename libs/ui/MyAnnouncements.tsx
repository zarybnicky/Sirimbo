import * as React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { AnnouncementItem } from './AnnouncementItem';
import { MyAnnouncementsDocument } from '@app/graphql/Announcement';
import { useQuery } from 'urql';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import { cn } from '@app/ui/cn';

const options = [
  { label: 'Aktuální', id: 'current' },
  { label: 'Archiv příspěvků', id: 'archive' },
]

export function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [state, setState] = React.useState('current');
  const [{ data }] = useQuery({
    query: MyAnnouncementsDocument,
    variables: { first: 5, offset: (page - 1) * 5, archive: state === 'archive' },
  });

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-4">Aktuality</h4>

      <ToggleGroupPrimitive.Root
        value={state}
        onValueChange={setState}
        type="single"
      >
        {options.map(({ label, id }) => (
          <ToggleGroupPrimitive.Item
            key={`group-item-${id}-${label}`}
            value={id}
            className={cn(
              'group data-[state=on]:bg-neutral-5 bg-white text-neutral-11',
              'border-y px-2.5 py-2 first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x',
              'border-neutral-5 data-[state=on]:border-neutral-8',
              'focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-neutral-8',
            )}
          >
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>

      <div className="space-y-2 rounded-lg mt-4">
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
