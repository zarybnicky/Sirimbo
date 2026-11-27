import { Archive, Pencil, Pin, PinOff, Send, Trash2 } from 'lucide-react';
import { type ActionContext, defineActions } from '@/lib/actions';
import {
  type AnnouncementFragment,
  DeleteAnnouncementDocument,
  UpdateAnnouncementDocument,
} from '@/graphql/Announcement';

export const announcementActions = defineActions<AnnouncementFragment>()([
  {
    id: 'announcement.edit',
    label: 'Upravit',
    icon: Pencil,
    visible: ({ auth, item }) => auth.canManageAnnouncement(item),
    href: ({ item }) => `/nastenka/${item.id}/edit`,
  },
  {
    id: 'announcement.sticky',
    label: ({ item }) => (item.isSticky ? 'Odepnout' : 'Připnout'),
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.isSticky ? PinOff : Pin,
    visible: ({ auth, item }) => auth.canManageAnnouncement(item),
    execute: async ({ item, mutate }) => {
      await mutate(UpdateAnnouncementDocument, {
        id: item.id,
        patch: { isSticky: !item.isSticky },
      });
    },
  },
  {
    id: 'announcement.status',
    label: ({ item }) => (item.status === 'PUBLISHED' ? 'Archivovat' : 'Zveřejnit'),
    icon: ({ item }: ActionContext<AnnouncementFragment>) =>
      item.status === 'PUBLISHED' ? Archive : Send,
    visible: ({ auth, item }) => auth.canManageAnnouncement(item),
    execute: async ({ item, mutate }) => {
      await mutate(UpdateAnnouncementDocument, {
        id: item.id,
        patch: { status: item.status === 'PUBLISHED' ? 'ARCHIVED' : 'PUBLISHED' },
      });
    },
  },
  {
    id: 'announcement.delete',
    label: 'Smazat',
    icon: Trash2,
    variant: 'danger',
    visible: ({ auth, item }) => auth.canManageAnnouncement(item),
    confirm: ({ item }) => ({
      description: `Opravdu chcete smazat příspěvek "${item.title}"?`,
    }),
    execute: async ({ item: { id }, mutate, router }) => {
      await mutate(DeleteAnnouncementDocument, { id });
      if (router.pathname === `/nastenka/${id}`) {
        router.replace('/nastenka');
      }
    },
  },
]);
