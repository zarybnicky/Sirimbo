import { useTypedQuery } from './zeus/apollo';

export const mockMenu: MenuType = [
  {
    "type": "menu", "text": "Klub", "hrefRoot": "/o-nas", "children": [
      { "type": "link", "text": "O nás", "href": "/o-nas" },
      { "type": "link", "text": "Kde trénujeme", "href": "/o-nas/kde-trenujeme" },
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
  { "type": "link", "text": "Aktuality", "href": "/aktualne" },
  { "type": "link", "text": "Galerie", "href": "/galerie" },
  { "type": "link", "text": "Akce", "href": "/akce" },
  { "type": "link", "text": "Kontakt", "href": "/kontakt" }
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

export const useMenu = (): MenuType => {
  const { data } = useTypedQuery({
    parameterByPaName: [
      { paName: "menu" },
      { paValue: true },
    ],
  });
  if (!data?.parameterByPaName?.paValue) {
    return [];
  }
  const menu = JSON.parse(data.parameterByPaName.paValue) as MenuType;
  return menu.filter(x => x.type as unknown !== 'text');
}
