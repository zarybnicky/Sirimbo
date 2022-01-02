import { useLocation } from 'react-router';
import { useTypedQuery } from '../zeus/apollo';
import { useAuth } from './use-auth';

export type MenuStructItem = {
  type: 'menu';
  text: string;
  children: MenuStructItem[];
} | {
  type: 'link';
  text: string;
  href: string;
};

export function useDbMenu(): MenuStructItem[] {
  const { data } = useTypedQuery({
    parameterByPaName: [
      { paName: "menu" },
      { paValue: true },
    ],
  });
  if (!data?.parameterByPaName?.paValue) {
    return [];
  }
  return JSON.parse(data.parameterByPaName.paValue) as MenuStructItem[];
};

export function getHrefs(x: MenuStructItem): string[] {
  return x.type === 'link' ? [x.href] : ([] as string[]).concat(...x.children.map(getHrefs));
}

export function useMenu(): MenuStructItem[] {
  const { user } = useAuth();
  const { pathname } = useLocation();
  if (!user) {
    return publicMenu;
  }
  const memberOrAdmin = user.permissionByUGroup?.peNastenka == 16 ?
    memberMenu.concat([{ type: 'menu', text: 'Administrace', children: adminMenu }]) : memberMenu;
  if (publicMenu.find(x => getHrefs(x).find(y => pathname.startsWith(y)))) {
    return publicMenu.concat([{ type: 'menu', text: 'Pro členy', children: memberOrAdmin }]);
  } else {
    return [{ type: 'menu', text: 'Pro veřejnost', children: publicMenu } as MenuStructItem].concat(memberOrAdmin);
  }
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
  { "type": "link", "text": "Akce", "href": "/events" },
  { "type": "link", "text": "Kontakt", "href": "/contact" }
];

export const memberMenu: MenuStructItem[] = [
  { "type": "link", "text": "Nástěnka", "href": "/dashboard" },
  { "type": "link", "text": "Tréninky", "href": "/schedule" },
  { "type": "link", "text": "Akce", "href": "/events" },
  { "type": "link", "text": "Dokumenty", "href": "/documents" },
  { "type": "link", "text": "Členové", "href": "/groups" },
  { "type": "link", "text": "Profil", "href": "/profile" },
];

export const adminMenu: MenuStructItem[] = [
  { "type": "link", "text": "Správa obsahu", "href": "/editor" },
  { "type": "link", "text": "Stará administrace", "href": "/admin/rozpis" },
];
