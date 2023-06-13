import * as React from 'react';
import { Pagination } from './Pagination';
import { AnnouncementItem } from './AnnouncementItem';
import { MyAnnouncementsDocument } from '@app/graphql/Announcement';
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

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-10">Aktuality</h4>

      <div className="space-y-2 rounded-lg">
        {data.myAnnouncements.nodes.map((a) => (
          <AnnouncementItem key={a.id} item={a} />
        ))}
      </div>

      <Pagination
        total={data.myAnnouncements.totalCount}
        limit={5}
        page={page}
        setPage={setPage}
      />
    </div>
  );
}
