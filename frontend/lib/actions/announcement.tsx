import { Eye, EyeOff, Pin, PinOff, Trash2 } from 'lucide-react';
import { Action, type ActionContext } from '@/lib/actions';
import {
  type AnnouncementFragment,
  DeleteAnnouncementDocument,
  ToggleAnnouncementStickyDocument,
  ToggleAnnouncementVisibleDocument,
} from '@/graphql/Announcement';

export const announcementActions: Action<AnnouncementFragment>[] = [
  {
    id: 'announcement.sticky',
    label: ({ item }) => (item.isSticky ? 'Odepnout' : 'Připnout'),
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.isSticky ? PinOff : Pin,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    execute: async ({ item, mutate }) => {
      await mutate(ToggleAnnouncementStickyDocument, {
        id: item.id,
        sticky: !item.isSticky,
      });
    },
  },
  {
    id: 'announcement.visible',
    label: ({ item }) => (item.isVisible ? 'Skrýt' : 'Zviditelnit'),
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.isVisible ? EyeOff : Eye,
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    execute: async ({ item, mutate }) => {
      await mutate(ToggleAnnouncementVisibleDocument, {
        id: item.id,
        visible: !item.isVisible,
      });
    },
  },
  {
    id: 'announcement.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth }) => auth.isAdmin,
    type: 'mutation',
    confirm: ({ item }) => ({
      description: `Opravdu chcete smazat příspěvek "${item.title}"?`,
    }),
    execute: async ({ item, mutate, router }) => {
      await mutate(DeleteAnnouncementDocument, { id: item.id });
      if (router.pathname === '/nastenka/[id]') {
        await router.replace('/nastenka');
      }
    },
  },
];
