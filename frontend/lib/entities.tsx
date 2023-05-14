import { DropdownItem } from 'components/Dropdown';
import { AnnouncementFragment } from 'lib/graphql/Announcement';
import { usePermissions } from './data/use-permissions';
import { CohortBasicFragment } from './graphql/Cohorts';
import { EventFragment } from './graphql/Event';
import { ReservationBasicFragment } from './graphql/Reservation';
import { ScheduleBasicFragment } from './graphql/Schedule';

export const Announcement = {
  useMenu(item: AnnouncementFragment): DropdownItem[] {
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

export const Schedule = {
  useMenu(item: ScheduleBasicFragment): DropdownItem[] {
    const perms = usePermissions();
    if (!perms.canEditSchedule(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/rozpis/[id]', query: { id: item.id } },
      },
    ];
  }
};

export const Reservation = {
  useMenu(item: ReservationBasicFragment): DropdownItem[] {
    const perms = usePermissions();
    if (!perms.canEditReservation(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/nabidka/[id]', query: { id: item.id } },
      },
    ];
  }
};

export const Event = {
  useMenu(item: EventFragment): DropdownItem[] {
    const perms = usePermissions();
    if (!perms.canEditEvent(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/akce/[id]', query: { id: item.id } },
      },
    ];
  }
};

export const Cohort = {
  useMenu(item: CohortBasicFragment): DropdownItem[] {
    const perms = usePermissions();
    if (!perms.canEditCohort(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/skupiny/[id]', query: { id: item.id } },
      },
    ];
  }
};
