import { gql } from 'graphql-tag';
import { useQuery } from '@apollo/client';

export const mockMenu: MenuType = [
  {
    "type": "menu", "text": "Klub", "children": [
      { "type": "link", "text": "O nás", "href": "/o-nas" },
      { "type": "link", "text": "Trenéři", "href": "/treneri" },
      { "type": "link", "text": "Tréninkové skupiny", "href": "/treninkove-skupiny" },
      { "type": "link", "text": "Benefity členství", "href": "/clenstvi" },
      { "type": "link", "text": "Galerie mistrů", "href": "/galerie-mistru" }
    ]
  },
  {
    "type": "menu", "text": "Nabízíme", "children": [
      { "type": "link", "text": "Tréninkové programy", "href": "/treninkove-programy" },
      { "type": "link", "text": "Školní taneční kroužky", "href": "/skolni-krouzky" },
      { "type": "link", "text": "Vystoupení na akcích", "href": "/vystoupení" }
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
