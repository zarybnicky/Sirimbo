import * as React from 'react';
import { useAnnouncementListQuery } from 'lib/graphql/Announcement';
import { Pagination } from './Pagination';
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

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-4">Aktuality</h4>

      <div className="flex flex-col gap-2 rounded-lg">
      {nodes.map((a) => (
        <AnnouncementItem key={a.id} item={a} />
      ))}
      </div>

      <Pagination {...{ total, limit, page, setPage }} />
    </div>
  );
}
