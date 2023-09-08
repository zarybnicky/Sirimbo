import * as React from 'react';
import { AnnouncementItem } from './AnnouncementItem';
import { StickyAnnouncementsDocument } from '@app/graphql/Announcement';
import { useQuery } from 'urql';

export function StickyAnnouncements() {
  const [{ data }] = useQuery({
    query: StickyAnnouncementsDocument,
  });
  if (!data?.stickyAnnouncements) {
    // react-skeleton
    return null;
  }

  return (
    <div className="flex flex-col">
      <h4 className="text-2xl tracking-wide mb-5">Stálá nástěnka</h4>
      <div className="space-y-2 rounded-lg">
        {data.stickyAnnouncements.nodes.map((a) => (
          <AnnouncementItem key={a.id} item={a} hideAll />
        ))}
      </div>
    </div>
  );
}
