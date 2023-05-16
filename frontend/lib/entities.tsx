import { DropdownItem } from 'components/Dropdown';
import { AnnouncementFragment } from 'lib/graphql/Announcement';
import { useAuth } from './data/use-auth';
import { CohortBasicFragment } from './graphql/Cohorts';
import { EventFragment } from './graphql/Event';
import { ReservationBasicFragment } from './graphql/Reservation';
import { ToggleUpozorneniVisibleDocument } from './graphql/Announcement';
import { ScheduleBasicFragment } from './graphql/Schedule';
import { useGqlMutation } from './query';
import { useQueryClient } from '@tanstack/react-query';

export const Announcement = {
  useMenu(item: AnnouncementFragment): DropdownItem[] {
    const { perms } = useAuth();
    const queryClient = useQueryClient();
    const hideMutation = useGqlMutation(ToggleUpozorneniVisibleDocument, {
      onSuccess() {
        queryClient.invalidateQueries(['MyAnnouncements']);
      },
    });
    if (!perms.canEditAnnouncement(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/nastenka/[id]', query: { id: item.id } },
      },
      {
        title: 'Skrýt',
        onClick: () => hideMutation.mutateAsync({ id: item.id, visible: false }),
      },
    ];
  },
};

export const Schedule = {
  useMenu(item: ScheduleBasicFragment): DropdownItem[] {
    const { perms } = useAuth();
    if (!perms.canEditSchedule(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/rozpis/[id]', query: { id: item.id } },
      },
    ];
  },
};

export const Reservation = {
  useMenu(item: ReservationBasicFragment): DropdownItem[] {
    const { perms } = useAuth();
    if (!perms.canEditReservation(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/nabidka/[id]', query: { id: item.id } },
      },
    ];
  },
};

export const Event = {
  useMenu(item: EventFragment): DropdownItem[] {
    const { perms } = useAuth();
    if (!perms.canEditEvent(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/akce/[id]', query: { id: item.id } },
      },
    ];
  },
};

export const Cohort = {
  useMenu(item: CohortBasicFragment): DropdownItem[] {
    const { perms } = useAuth();
    if (!perms.canEditCohort(item)) {
      return [];
    }
    return [
      {
        title: 'Upravit',
        href: { pathname: '/admin/skupiny/[id]', query: { id: item.id } },
      },
    ];
  },
};
