import { useAuth } from './use-auth';
import { PermissionKey, PermissionLevel, usePermissions } from './use-permissions';

export type MenuLink = {
  type: 'link';
  title: string;
  href: string;
  auth?: [PermissionKey, PermissionLevel];
};

export type MenuStructItem = {
  type: 'menu';
  hrefRoot?: string;
  title: string;
  children: MenuLink[];
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
  const permissions = usePermissions();
  return (permissions.hasPermission(PermissionKey.peNastenka, PermissionLevel.P_OWNED)) ? [{
    type: 'menu',
    title: 'Správa',
    children: adminMenu.filter(item => !item.auth || permissions.hasPermission(...item.auth)),
  }] : [];
}

const publicMenu: MenuStructItem[] = [
  {
    type: "menu", title: "Klub", children: [
      { type: "link", title: "O nás", "href": "/o-nas" },
      { type: "link", title: "Kde trénujeme", "href": "/o-nas/kde-trenujeme" },
      { type: "link", title: "Trenéři", "href": "/o-nas/treneri" },
      { type: "link", title: "Tréninkové skupiny", "href": "/o-nas/treninkove-skupiny" },
      { type: "link", title: "Benefity členství", "href": "/o-nas/clenstvi" },
      { type: "link", title: "Galerie mistrů", "href": "/o-nas/galerie-mistru" }
    ]
  },
  {
    type: "menu", title: "Nabízíme", children: [
      { type: "link", title: "Tréninkové programy", "href": "/nabizime/treninkove-programy" },
      { type: "link", title: "Školní taneční kroužky", "href": "/nabizime/skolni-krouzky" },
      { type: "link", title: "Vystoupení na akcích", "href": "/nabizime/vystoupení" }
    ]
  },
  { type: "link", title: "Aktuality", "href": "/articles" },
  { type: "link", title: "Galerie", "href": "/gallery" },
  { type: "link", title: "Akce", "href": "/events/public" },
  { type: "link", title: "Kontakt", "href": "/contact" }
];

const adminMenu: MenuLink[] = [
  {
    type: "link", title: 'Uživatelé', href: '/admin/users',
    auth: [PermissionKey.peUsers, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Skupiny', href: '/admin/skupiny',
    auth: [PermissionKey.peSkupiny, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Platby', href: '/admin/platby',
    auth: [PermissionKey.pePlatby, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Páry', href: '/admin/pary',
    auth: [PermissionKey.pePary, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Články', href: '/admin/aktuality',
    auth: [PermissionKey.peAktuality, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Nástěnka', href: '/admin/nastenka',
    auth: [PermissionKey.peNastenka, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Rozpis', href: '/admin/rozpis',
    auth: [PermissionKey.peRozpis, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Nabídka', href: '/admin/nabidka',
    auth: [PermissionKey.peNabidka, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Akce', href: '/admin/akce',
    auth: [PermissionKey.peAkce, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Galerie', href: '/admin/galerie',
    auth: [PermissionKey.peGalerie, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Video', href: '/admin/video',
    auth: [PermissionKey.peAktuality, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Dokumenty', href: '/admin/dokumenty',
    auth: [PermissionKey.peDokumenty, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: 'Oprávnění', href: '/admin/permissions',
    auth: [PermissionKey.pePermissions, PermissionLevel.P_OWNED]
  },
  {
    type: "link", title: "Správa obsahu", href: "/admin/page",
    auth: [PermissionKey.peNastenka, PermissionLevel.P_ADMIN],
  },
  {
    type: "link", title: "Zájemci", href: "/crm",
    auth: [PermissionKey.peNastenka, PermissionLevel.P_ADMIN],
  },
];
