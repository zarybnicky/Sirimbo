import { useAuth } from '@/lib/auth';
import type { AccessRequirements } from '@/lib/auth-claims';
import { useAtomValue } from 'jotai';
import { calendarPreferencesAtom } from '@/calendar/state';
import {
  Broom,
  CalendarDays,
  Clipboard,
  Coins,
  createLucideIcon,
  Crown,
  FileInput,
  Footprints,
  Globe,
  KeyRound,
  type LucideIcon,
  Mail,
  Newspaper,
  Pin,
  Star,
  University,
  UserGroup,
  UserRound,
} from 'lucide-react';

export const DanceCouple = createLucideIcon('dance-couple', [
  ['circle', { cx: 8, cy: 5, r: 2, key: 'left-head' }],
  ['circle', { cx: 16, cy: 6, r: 2, key: 'right-head' }],
  [
    'path',
    {
      d: 'm3 9 5 2 4-1 4 2 5-3M8 11l-1 5-3 4m3-4 4 4m5-8 1 4 4 4m-4-4-3 4',
      key: 'pose',
    },
  ],
]);

export type MenuLink = AccessRequirements & {
  type?: never;
  title: string;
  href: string;
  icon?: LucideIcon;
  className?: string;
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export const topMenu: MenuStructItem[] = [
  { title: 'Domů', href: '/' },
  {
    type: 'menu',
    title: 'Klub',
    children: [
      { title: 'O nás', href: '/o-nas' },
      { title: 'Kde trénujeme', href: '/kde-trenujeme' },
      { title: 'Tréninkové programy', href: '/treninkove-programy' },
      { title: 'Trenéři', href: '/treneri' },
      { title: 'Výhody členství', href: '/vyhody-clenstvi' },
      { title: 'Galerie mistrů', href: '/galerie-mistru' },
    ],
  },
  {
    type: 'menu',
    title: 'Nabízíme',
    children: [
      {
        title: 'Přípravka tanečního sportu',
        href: 'https://nabor.tkolymp.cz',
      },
      { title: 'Vystoupení na akcích', href: '/vystoupeni' },
      { title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
    ],
  },
  { title: 'Aktuality', href: '/clanky' },
  { title: 'Galerie', href: '/galerie' },
  { title: 'Akce', href: '/akce' },
  { title: 'Kontakt', href: '/kontakt' },
];

const memberMenu: MenuStructItem[] = [
  {
    title: 'Přehled',
    href: '/dashboard?tab=myAnnouncements',
    icon: Star,
  },
  {
    title: 'Nástěnka',
    href: '/nastenka',
    icon: Mail,
    requireMember: true,
  },
  {
    title: 'Stálá nástěnka',
    className: 'lg:hidden',
    href: '/dashboard?tab=stickyAnnouncements',
    icon: Pin,
    requireMember: true,
    requirePublicSite: true,
  },
  { title: 'Kalendář', href: '/rozpis', icon: CalendarDays },
  { title: 'Soustředění', href: '/akce', icon: Footprints },
  {
    type: 'menu',
    title: 'Taneční klub',
    children: [
      {
        title: 'Klub',
        href: '/tanecni-klub',
        icon: University,
        requireMember: true,
      },
      {
        title: 'Tréninkové skupiny',
        href: '/treninkove-skupiny',
        icon: UserGroup,
      },
      {
        title: 'Páry',
        href: '/pary',
        icon: DanceCouple,
        requireMember: true,
      },
      {
        title: 'Členové',
        href: '/clenove',
        icon: UserRound,
        requireMember: true,
      },
      {
        title: 'Žebříček',
        href: '/zebricek',
        icon: Crown,
        requireMember: true,
      },
      {
        title: 'Soutěžní data',
        href: '/federated',
        icon: Globe,
        requireMember: true,
      },
    ],
  },
  {
    type: 'menu',
    title: 'Správa',
    children: [
      {
        title: 'Platby',
        href: '/platby',
        icon: Coins,
        requireAdmin: true,
      },
      {
        title: 'Články',
        href: '/aktuality',
        icon: Newspaper,
        requireTrainer: true,
        requirePublicSite: true,
      },
      {
        title: 'Vyplněné formuláře',
        href: '/crm',
        icon: Clipboard,
        requireAdmin: true,
        requirePublicSite: true,
      },
      {
        title: 'Přístupy',
        href: '/pristupy',
        icon: KeyRound,
        requireAdmin: true,
      },
      {
        title: 'Import z evidence',
        href: '/starlet-import',
        icon: FileInput,
        requireAdmin: true,
        requireStarletImport: true,
      },
      {
        title: 'Crawler',
        href: '/crawler',
        icon: Broom,
        requireSystemAdmin: true,
      },
    ],
  },
];

export function useMemberMenu(): MenuStructItem[] {
  const { canAccess } = useAuth();
  const preferences = useAtomValue(calendarPreferencesAtom);
  const params = new URLSearchParams(preferences);
  const calendarHref = `/rozpis${params.size > 0 ? `?${params}` : ''}`;

  const visibleMenu: MenuStructItem[] = [];
  for (const item of memberMenu) {
    if (item.type === 'menu') {
      const children = item.children.filter(canAccess);
      if (children.length > 0) visibleMenu.push({ ...item, children });
    } else if (canAccess(item)) {
      visibleMenu.push(item);
    }
  }

  return visibleMenu.map((item) =>
    item.type !== 'menu' && item.href === '/rozpis'
      ? { ...item, href: calendarHref }
      : item,
  );
}
