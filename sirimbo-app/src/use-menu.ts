import { gql } from 'graphql-tag';
import { useQuery } from '@apollo/client';

export const mockMenu: MenuType = [
  {
    "type": "menu", "text": "Klub", "hrefRoot": "/o-nas", "children": [
      { "type": "link", "text": "O nás", "href": "/o-nas" },
      { "type": "link", "text": "Trenéři", "href": "/o-nas/treneri" },
      { "type": "link", "text": "Tréninkové skupiny", "href": "/o-nas/treninkove-skupiny" },
      { "type": "link", "text": "Benefity členství", "href": "/o-nas/clenstvi" },
      { "type": "link", "text": "Galerie mistrů", "href": "/o-nas/galerie-mistru" }
    ]
  },
  {
    "type": "menu", "text": "Nabízíme", "hrefRoot": "/nabizime", "children": [
      { "type": "link", "text": "Tréninkové programy", "href": "/nabizime/treninkove-programy" },
      { "type": "link", "text": "Školní taneční kroužky", "href": "/nabizime/skolni-krouzky" },
      { "type": "link", "text": "Vystoupení na akcích", "href": "/nabizime/vystoupení" }
    ]
  },
  { "type": "link", "text": "Aktuality", "href": "/news" },
  { "type": "link", "text": "Galerie", "href": "/gallery" },
  { "type": "link", "text": "Akce", "href": "/events" },
  { "type": "link", "text": "Kontakt", "href": "/contact" }
];

export type MenuType = (MenuLink | SubmenuType)[];
export interface SubmenuType {
  type: 'menu';
  text: string;
  hrefRoot: string;
  children: MenuLink[];
}
export interface MenuLink {
  type: 'link';
  text: string;
  href: string;
}

export const GetMenu = gql(`
query GetMenu {
parameters_by_pk(pa_name: "menu") {
    pa_value
  }
}`);

export const useMenu = (): MenuType => {
  const { data } = useQuery(GetMenu);
  if (!data?.parameters_by_pk?.pa_value) {
    return [];
  }
  const menu = JSON.parse(data?.parameters_by_pk?.pa_value) as MenuType;
  return menu.filter(x => x.type as unknown !== 'text');
}