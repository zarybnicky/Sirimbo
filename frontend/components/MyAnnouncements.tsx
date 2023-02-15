import * as React from 'react';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { useAnnouncementListQuery } from 'lib/graphql/Announcement';
import { Pagination } from './Pagination';
import { fullDateFormatter } from 'lib/format-date';
import { AnnouncementItem } from './AnnouncementItem';

export function MyAnnouncements() {
  const [limit] = React.useState(5);
  const [page, setPage] = React.useState(1);
  const { data } = useAnnouncementListQuery({ limit, offset: (page - 1) * limit });

  if (!data?.upozornenis) {
    // react-skeleton
    return null;
  }
  const nodes = data.upozornenis.nodes;
  const total = data.upozornenis.totalCount;
  const hasNext = total >= page * limit;
  const hasPrev = 0 < (page - 1) * limit;

  return <div className="flex flex-col items-center">
    <h4 className="text-2xl tracking-wide">Aktuality</h4>
    <div className="flex items-center mb-4">
      {hasPrev ? (
        <button className="button button-icon text-stone-500" onClick={() => setPage(page - 1)}>
          <ChevronLeft />
        </button>
      ) : (
        <button className="button button-icon text-stone-300">
          <ChevronLeft />
        </button>
      )}

      <span className="text-stone-500">
        {nodes.length > 0 ? fullDateFormatter.formatRange(
          new Date(nodes[0]!.upTimestampAdd),
          new Date(nodes[nodes.length - 1]!.upTimestampAdd),
        ) : ''}
      </span>

      {hasNext ? (
        <button className="button button-icon text-stone-500" onClick={() => setPage(page + 1)}>
          <ChevronRight />
        </button>
      ) : (
        <button className="button button-icon text-stone-300">
          <ChevronRight />
        </button>
      )}
    </div>

    {nodes.map((a) => <AnnouncementItem key={a.id} item={a} />)}

    <Pagination {...{ total, limit, page, setPage }} />
  </div>;
}
