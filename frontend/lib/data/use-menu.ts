import { useTypedQuery } from 'lib/zeus/apollo';
import { useRouter } from 'next/router';
import { useAuth } from './use-auth';

export type MenuStructItem = {
  type: 'menu';
  hrefRoot?: string;
  text: string;
  children: MenuStructItem[];
} | {
  type: 'link';
  text: string;
  href: string;
};

export function useDbMenu(): MenuStructItem[] {
  const { data } = useTypedQuery({
    parameter: [
      { paName: "menu" },
      { paValue: true },
    ],
  });
  if (!data?.parameter?.paValue) {
    return [];
  }
  return JSON.parse(data.parameter.paValue) as MenuStructItem[];
};

export function getHrefs(x: MenuStructItem): string[] {
  return x.type === 'link'
    ? [x.href]
    : (x.hrefRoot ? [x.hrefRoot] : []).concat(...x.children.map(getHrefs));
}

export function useMenu(): MenuStructItem[] {
  const { user } = useAuth();
  const { pathname } = useRouter();
  if (!user) {
    return publicMenu;
  }
  const isAdmin = user.permissionByUGroup?.peNastenka == 16;

  if (getHrefs(publicSubmenu).find(y => pathname.startsWith(y))) {
    return publicMenu.concat([memberSubmenu]).concat(isAdmin ? [adminSubmenu] : []);
  }
  if (getHrefs(adminSubmenu).find(y => pathname.startsWith(y))) {
    return [publicSubmenu].concat([memberSubmenu]).concat(isAdmin ? adminMenu : []);
  }
  return [publicSubmenu].concat(memberMenu).concat(isAdmin ? [adminSubmenu] : []);
}

export const publicMenu: MenuStructItem[] = [
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

export const memberMenu: MenuStructItem[] = [
  { "type": "link", "text": "Nástěnka", "href": "/dashboard" },
  { "type": "link", "text": "Tréninky", "href": "/schedule" },
  { "type": "link", "text": "Akce", "href": "/events/private" },
  { "type": "link", "text": "Dokumenty", "href": "/documents" },
  { "type": "link", "text": "Členové", "href": "/groups" },
  { "type": "link", "text": "Profil", "href": "/profile" },
];

export const adminMenu: MenuStructItem[] = [
  { "type": "link", "text": "Správa obsahu", "href": "/editor" },
  { "type": "link", "text": "Zájemci", "href": "/crm" },
  { "type": "link", "text": "Stará administrace", "href": "/admin/rozpis" },
];

const publicSubmenu: MenuStructItem = {
  type: 'menu',
  hrefRoot: '/home',
  text: 'Pro veřejnost',
  children: publicMenu,
};

const memberSubmenu: MenuStructItem = {
  type: 'menu',
  text: 'Pro členy',
  children: memberMenu,
};

const adminSubmenu: MenuStructItem = {
  type: 'menu',
  text: 'Správa',
  children: adminMenu,
};
