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
import { Route } from 'nextjs-routes';

export const Article = {
  name: (n: number) => (n === 1 ? 'článek' : n > 1 && n < 5 ? 'články' : 'článků'),
  addRoute: { pathname: '/admin/aktuality/add' } as Route,
  editRoute: (id: string) =>
    ({ pathname: '/admin/aktuality/[id]', query: { id } } as Route),
}

export const Couple = {
  name: (n: number) => (n === 1 ? "pár" : n > 1 && n < 5 ? "páry":"párů"),
  addRoute: { pathname: '/admin/pary/add' } as Route,
  editRoute: (id: string) =>
    ({ pathname: '/admin/pary/[id]', query: { id } } as Route),
};

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
  name: (n: number) => (n === 1 ? 'rozpis' : n > 1 && n < 5 ? 'rozpisy' : 'rozpisů'),
  addRoute: { pathname: '/admin/rozpis/add' } as Route,
  editRoute: (id: string) =>
    ({ pathname: '/admin/rozpis/[id]', query: { id } } as Route),
  useMenu(item?: ScheduleBasicFragment): DropdownItem[] {
    const { perms } = useAuth();
    if (!item || !perms.canEditSchedule(item)) {
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
  name: (n: number) => (n === 1 ? 'nabídka' : n > 1 && n < 5 ? 'nabídky' : 'nabídek'),
  addRoute: { pathname: '/admin/nabidka/add' } as Route,
  editRoute: (id: string) =>
    ({ pathname: '/admin/nabidka/[id]', query: { id } } as Route),
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
  name: (n: number) => (n === 1 ? 'akce' : n > 1 && n < 5 ? 'akce' : 'akcí'),
  addRoute: { pathname: '/admin/akce/add' } as Route,
  editRoute: (id: string) =>
    ({ pathname: '/admin/akce/[id]', query: { id } } as Route),
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
