import { useAuth } from './use-auth';
import { PermissionKey, PermissionLevel, usePermissions } from './use-permissions';

type MenuLink = {
  type: 'link';
  text: string;
  href: string;
  auth?: [PermissionKey, PermissionLevel];
};
export type MenuStructItem = {
  type: 'menu';
  hrefRoot?: string;
  text: string;
  children: MenuStructItem[];
} | MenuLink;

export function getHrefs(x: MenuStructItem): string[] {
  return x.type === 'link'
    ? [x.href]
    : (x.hrefRoot ? [x.hrefRoot] : []).concat(...x.children.map(getHrefs));
}

export function useTopMenu(): MenuStructItem[] {
  return publicMenu;
}

export function useSideMenu(): MenuStructItem[] {
  const { user } = useAuth();
  const permissions = usePermissions();

  const menu: MenuStructItem[] = [];
  if (user) {
    menu.push({
      type: 'menu',
      text: 'Pro členy',
      children: memberMenu,
    });
  }
  if (permissions.hasPermission(PermissionKey.peNastenka, PermissionLevel.P_OWNED)) {
    menu.push({
      type: 'menu',
      text: 'Správa',
      children: adminMenu.filter(item => !item.auth || permissions.hasPermission(...item.auth)),
    });
  }
  return menu;
}

const publicMenu: MenuStructItem[] = [
  {
    "type": "menu", "text": "Klub", "children": [
      { "type": "link", "text": "O nás", "href": "/o-nas" },
      { "type": "link", "text": "Kde trénujeme", "href": "/o-nas/kde-trenujeme" },
      { "type": "link", "text": "Trenéři", "href": "/o-nas/treneri" },
      { "type": "link", "text": "Tréninkové skupiny", "href": "/o-nas/treninkove-skupiny" },
      { "type": "link", "text": "Benefity členství", "href": "/o-nas/clenstvi" },
      { "type": "link", "text": "Galerie mistrů", "href": "/o-nas/galerie-mistru" }
    ]
  },
  {
    "type": "menu", "text": "Nabízíme", "children": [
      { "type": "link", "text": "Tréninkové programy", "href": "/nabizime/treninkove-programy" },
      { "type": "link", "text": "Školní taneční kroužky", "href": "/nabizime/skolni-krouzky" },
      { "type": "link", "text": "Vystoupení na akcích", "href": "/nabizime/vystoupení" }
    ]
  },
  { "type": "link", "text": "Aktuality", "href": "/articles" },
  { "type": "link", "text": "Galerie", "href": "/gallery" },
  { "type": "link", "text": "Akce", "href": "/events/public" },
  { "type": "link", "text": "Kontakt", "href": "/contact" }
];

const memberMenu: MenuLink[] = [
  { "type": "link", "text": 'Nástěnka', "href": '/dashboard' },
  { "type": "link", "text": 'Tréninky', "href": '/schedule' },
  { "type": "link", "text": 'Akce', "href": '/events' },
  { "type": "link", "text": 'Dokumenty', "href": '/documents' },
  { "type": "link", "text": 'Členové', "href": '/cohorts' },
  { "type": "link", "text": 'Profil', "href": '/profile' },
];

const adminMenu: MenuLink[] = [
  {
    "type": "link", "text": 'Uživatelé', "href": '/admin/users',
    auth: [PermissionKey.peUsers, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Skupiny', "href": '/admin/skupiny',
    auth: [PermissionKey.peSkupiny, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Platby', "href": '/admin/platby',
    auth: [PermissionKey.pePlatby, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Páry', "href": '/admin/pary',
    auth: [PermissionKey.pePary, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Články', "href": '/admin/aktuality',
    auth: [PermissionKey.peAktuality, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Nástěnka', "href": '/admin/nastenka',
    auth: [PermissionKey.peNastenka, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Rozpis', "href": '/admin/rozpis',
    auth: [PermissionKey.peRozpis, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Nabídka', "href": '/admin/nabidka',
    auth: [PermissionKey.peNabidka, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Akce', "href": '/admin/akce',
    auth: [PermissionKey.peAkce, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Galerie', "href": '/admin/galerie',
    auth: [PermissionKey.peGalerie, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Video', "href": '/admin/video',
    auth: [PermissionKey.peAktuality, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Dokumenty', "href": '/admin/dokumenty',
    auth: [PermissionKey.peDokumenty, PermissionLevel.P_OWNED]
  },
  {
    "type": "link", "text": 'Oprávnění', "href": '/admin/permissions',
    auth: [PermissionKey.pePermissions, PermissionLevel.P_OWNED]
  },
  { "type": "link", "text": "Správa obsahu", "href": "/editor" },
  { "type": "link", "text": "Zájemci", "href": "/crm" },
];
