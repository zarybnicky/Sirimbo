import * as React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { AnnouncementItem } from './AnnouncementItem';
import { MyAnnouncementsDocument } from '@app/graphql/Announcement';
import { useQuery } from 'urql';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import { cn } from '@app/ui/cn';
import { typographyCls } from './style';

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
      <div className="mb-4 flex gap-6 flex-wrap grow-0 h-min justify-between items-baseline relative">
        <h1 className={typographyCls({ variant: 'smallHeading', className: 'mt-0' })}>
          Aktuality
        </h1>

        <ToggleGroupPrimitive.Root value={state} onValueChange={setState} type="single" className="grow">
          {options.map(({ label, id }) => (
            <ToggleGroupPrimitive.Item
              key={`group-item-${id}-${label}`}
              value={id}
              className={cn(
                'group data-[state=on]:bg-neutral-5 bg-neutral-3 text-neutral-11',
                'border-y px-2 py-1 text-sm first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x',
                'border-neutral-5 data-[state=on]:border-neutral-8',
                'focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-neutral-8',
              )}
            >
              {label}
            </ToggleGroupPrimitive.Item>
          ))}
        </ToggleGroupPrimitive.Root>
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
