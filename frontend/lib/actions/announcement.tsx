import { Archive, Pin, PinOff, Send, Trash2 } from 'lucide-react';
import { type ActionContext, defineActions } from '@/lib/actions';
import {
  type AnnouncementFragment,
  DeleteAnnouncementDocument,
  SetAnnouncementStatusDocument,
  ToggleAnnouncementStickyDocument,
} from '@/graphql/Announcement';

export function canManageAnnouncement({ auth, item }: ActionContext<AnnouncementFragment>) {
  return auth.isAdmin || (auth.isTrainer && item.author?.id === auth.user?.id);
}

export const announcementActions = defineActions<AnnouncementFragment>()([
  {
    id: 'announcement.sticky',
    label: ({ item }) => (item.isSticky ? 'Odepnout' : 'Připnout'),
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.isSticky ? PinOff : Pin,
    visible: canManageAnnouncement,
    execute: async ({ item, mutate }) => {
      await mutate(ToggleAnnouncementStickyDocument, {
        id: item.id,
        sticky: !item.isSticky,
      });
    },
  },
  {
    id: 'announcement.status',
    label: ({ item }) =>
      item.status === 'PUBLISHED' ? 'Archivovat' : 'Zveřejnit',
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.status === 'PUBLISHED' ? Archive : Send,
    visible: canManageAnnouncement,
    execute: async ({ item, mutate }) => {
      await mutate(SetAnnouncementStatusDocument, {
        id: item.id,
        status: item.status === 'PUBLISHED' ? 'ARCHIVED' : 'PUBLISHED',
      });
    },
  },
  {
    id: 'announcement.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: canManageAnnouncement,
    confirm: ({ item }) => ({
      description: `Opravdu chcete smazat příspěvek "${item.title}"?`,
    }),
    execute: async ({ item: { id }, mutate, router }) => {
      await mutate(DeleteAnnouncementDocument, { id });
      if (router.pathname === `/nastenka/${id}`) {
        await router.replace('/nastenka');
      }
    },
  },
]);
