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

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-10">Aktuality</h4>

      <div className="mb-2 flex items-center">
        <div className="button-outline text-neutral-11">
          Aktuální
        </div>
        <div className="text-neutral-11">
          Archiv příspěvků
        </div>
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
