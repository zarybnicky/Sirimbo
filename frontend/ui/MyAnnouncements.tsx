import { AnnouncementItem } from '@/ui/AnnouncementItem';
import { Pagination } from '@/ui/Pagination';
import { cn } from '@/ui/cn';
import { MyAnnouncementsDocument } from '@/graphql/Announcement';
import { typographyCls, buttonCls, buttonGroupCls } from '@/ui/style';
import { numericDateWithYearFormatter, fullDateFormatter } from '@/ui/format';
import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import * as React from 'react';
import { useQuery } from 'urql';

const options = [
  { label: 'Aktuální', id: 'current' },
  { label: 'Archiv příspěvků', id: 'archive' },
]

export function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [state, setState] = React.useState('current');
  const [sort, setSort] = React.useState<'created' | 'updated'>('created');
  const orderByUpdated = sort === 'updated';

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

      <div className="flex flex-wrap items-center gap-2 mb-3">
        <span className="text-xs uppercase tracking-wide text-neutral-11">Seřadit podle:</span>
        <div className={buttonGroupCls({ className: 'shadow-none' })}>
          <button
            type="button"
            className={buttonCls({
              size: 'sm',
              variant: sort === 'created' ? 'primary' : 'outline',
            })}
            onClick={() => setSort('created')}
          >
            Data vytvoření
          </button>
          <button
            type="button"
            className={buttonCls({
              size: 'sm',
              variant: sort === 'updated' ? 'primary' : 'outline',
            })}
            onClick={() => setSort('updated')}
          >
            Poslední úpravy
          </button>
        </div>
      </div>

      <div className="space-y-2 rounded-lg">
        {(data?.myAnnouncements?.nodes || []).map((a) => (
          <AnnouncementItem
            key={a.id}
            item={a}
            renderDates={({ createdAt, updatedAt, wasUpdated }) => (
              <div className="flex items-center gap-1">
                <time dateTime={createdAt.toISOString()} title={fullDateFormatter.format(createdAt)}>
                  {numericDateWithYearFormatter.format(createdAt)}
                </time>
                {wasUpdated && (
                  <>
                    <span>-</span>
                    <time dateTime={updatedAt.toISOString()} title={fullDateFormatter.format(updatedAt)}>
                      Upraveno
                    </time>
                  </>
                )}
              </div>
            )}
          />
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
