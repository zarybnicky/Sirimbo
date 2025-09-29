import * as React from 'react';
import { AnnouncementItem } from '@/ui/AnnouncementItem';
import { StickyAnnouncementsDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { fullDateFormatter, numericDateWithYearFormatter } from '@/ui/format';

export function StickyAnnouncements() {
  const [sort, setSort] = React.useState<'created' | 'updated'>('created');
  const orderByUpdated = sort === 'updated';

  const [{ data }] = useQuery({
    query: StickyAnnouncementsDocument,
    variables: { orderByUpdated },
  });
  if (!data?.stickyAnnouncements) {
    // react-skeleton
    return null;
  }

  return (
    <div className="flex flex-col">
      <div className="flex flex-wrap items-center justify-between gap-3 mb-5">
        <h4 className="text-2xl tracking-wide">Stálá nástěnka</h4>
        <div className="flex flex-wrap items-center gap-2">
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
      </div>
      <div className="space-y-2 rounded-lg">
        {data.stickyAnnouncements.nodes.map((a) => (
          <AnnouncementItem
            key={a.id}
            item={a}
            onlyTitle
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
    </div>
  );
}
