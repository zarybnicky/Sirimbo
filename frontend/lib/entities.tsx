import { AnnouncementFragment } from 'lib/graphql/Announcement';
import { usePermissions } from './data/use-permissions';

export const Announcement = {
  useMenu(item: AnnouncementFragment) {
    const perms = usePermissions();
    if (!perms.canEditAnnouncement(item)) {
      return [];
    }

    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/nastenka/[id]', query: { id: item.id } },
      },
    ];
  }
}
