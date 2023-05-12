import { AnnouncementFragment } from 'lib/graphql/Announcement';
import { DropdownItem } from 'components/Dropdown';

export function getMenu(x: AnnouncementFragment): DropdownItem[] {
  return [
    { title: 'Upravit', href: { pathname: '/admin/nastenka/[id]', query: { id: x.id } } },
  ];
}
