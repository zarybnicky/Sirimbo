import * as React from 'react';
import { Pagination } from './Pagination';
import { AnnouncementItem } from './AnnouncementItem';
import { MyAnnouncementsDocument } from 'lib/graphql/Announcement';
import { useQuery } from 'urql';

export function MyAnnouncements() {
  const [page, setPage] = React.useState(1);
  const [{ data }] = useQuery({
    query: MyAnnouncementsDocument,
    variables: { first: 5, offset: (page - 1) * 5 },
  });
  if (!data?.myAnnouncements) {
    // react-skeleton
    return null;
  }
  const nodes = data.myAnnouncements.nodes;
  const total = data.myAnnouncements.totalCount;

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-10">Aktuality</h4>

      <div className="flex flex-col gap-2 rounded-lg">
        {nodes.map((a) => (
          <AnnouncementItem key={a.id} item={a} />
        ))}
      </div>

      <Pagination {...{ total, limit: 5, page, setPage }} />
    </div>
  );
}
