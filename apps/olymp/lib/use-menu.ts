import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';

export type MenuLink = {
  type: 'link';
  title: string;
  href: string;
  auth?: [PermissionKey, PermissionLevel];
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export function getHrefs(x: MenuStructItem): string[] {
  return x.type === 'link' ? [x.href] : x.children.flatMap(getHrefs);
}

export const topMenu: MenuStructItem[] = [
  { type: 'link', title: 'Domů', href: '/' },
  {
    type: 'menu',
    title: 'Klub',
    children: [
      { type: 'link', title: 'O nás', href: '/o-nas' },
      { type: 'link', title: 'Kde trénujeme', href: '/kde-trenujeme' },
      { type: 'link', title: 'Tréninkové programy', href: '/treninkove-programy' },
      { type: 'link', title: 'Trenéři', href: '/treneri' },
      { type: 'link', title: 'Výhody členství', href: '/vyhody-clenstvi' },
      { type: 'link', title: 'Galerie mistrů', href: '/galerie-mistru' },
    ],
  },
  {
    type: 'menu',
    title: 'Nabízíme',
    children: [
      { type: 'link', title: 'Vystoupení na akcích', href: '/vystoupeni' },
      { type: 'link', title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
    ],
  },
  { type: 'link', title: 'Aktuality', href: '/clanky' },
  { type: 'link', title: 'Galerie', href: '/galerie' },
  { type: 'link', title: 'Akce', href: '/akce' },
  { type: 'link', title: 'Kontakt', href: '/contact' },
];

export const memberMenu: MenuLink[] = [
  { type: 'link', title: 'Nástěnka', href: '/dashboard' },
  { type: 'link', title: 'Kalendář (WIP)', href: '/calendar' },
  { type: 'link', title: 'Tréninky', href: '/rozpis' },
  { type: 'link', title: 'Akce', href: '/akce' },
  { type: 'link', title: 'Dokumenty', href: '/dokumenty' },
  { type: 'link', title: 'Členové', href: '/treninkove-skupiny' },
  { type: 'link', title: 'Můj profil', href: '/profil' },
];

export const adminMenu: MenuLink[] = [
  {
    type: 'link',
    title: 'Organizace',
    href: '/admin/tenant',
    auth: [PermissionKey.peUsers, PermissionLevel.P_ADMIN],
  },
  {
    type: 'link',
    title: 'Uživatelé',
    href: '/admin/users',
    auth: [PermissionKey.peUsers, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Chci tančit!',
    href: '/crm',
    auth: [PermissionKey.peNastenka, PermissionLevel.P_ADMIN],
  },
  {
    type: 'link',
    title: 'Tréninkové programy',
    href: '/admin/cohort-group',
    auth: [PermissionKey.peSkupiny, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Skupiny',
    href: '/admin/cohorts',
    auth: [PermissionKey.peSkupiny, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Páry',
    href: '/admin/pary',
    auth: [PermissionKey.pePary, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Články',
    href: '/admin/aktuality',
    auth: [PermissionKey.peAktuality, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Nástěnka',
    href: '/admin/nastenka',
    auth: [PermissionKey.peNastenka, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Rozpis',
    href: '/admin/rozpis',
    auth: [PermissionKey.peRozpis, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Nabídka',
    href: '/admin/nabidka',
    auth: [PermissionKey.peNabidka, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Akce',
    href: '/admin/akce',
    auth: [PermissionKey.peAkce, PermissionLevel.P_OWNED],
  },
  {
    type: 'link',
    title: 'Upload (WIP)',
    href: '/admin/upload',
    auth: [PermissionKey.peNastenka, PermissionLevel.P_ADMIN],
  },
];
