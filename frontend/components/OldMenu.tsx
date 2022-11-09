import { NextLinkComposed } from './Link';
import { usePermissions, PermissionKey, PermissionLevel } from 'lib/data/use-permissions';

type NavbarItem
  = [string]
  | [string, string]
  | [string, string, NavbarItem[]]
  | [string, string, NavbarItem[], [PermissionKey, PermissionLevel]];

const topMenu: NavbarItem[] = [
  ['Klub', '/', [
    ['Kluboví trenéři', '/oklubu/klubovi-treneri'],
    ['Externí trenéři', '/oklubu/externi-treneri'],
    ['Kde trénujeme', '/oklubu/saly'],
  ]],
  ['Aktuality', '/aktualne'],
  ['Videa', '/video'],
  ['Fotogalerie', '/fotogalerie'],
  ['Kontakt', '/kontakt'],
];

// if logged in
const bottomMenu: NavbarItem[] = [
  ['Nástěnka', '/dashboard'],
  ['Tréninky', '/schedule'],
  ['Akce', '/member/akce'],
  ['Dokumenty', '/documents'],
  ['Členové', '/member/clenove'],
  ['Profil', '/member/profil'],
  ['w-100'],
  ['Administrace', '/admin', [
    ['Uživatelé', '/admin/users', [], [PermissionKey.peUsers, PermissionLevel.P_OWNED]],
    ['Skupiny', '/admin/skupiny', [], [PermissionKey.peSkupiny, PermissionLevel.P_OWNED]],
    ['Platby', '/admin/platby', [], [PermissionKey.pePlatby, PermissionLevel.P_OWNED]],
    ['Páry', '/admin/pary', [], [PermissionKey.pePary, PermissionLevel.P_OWNED]],
    ['Články', '/admin/aktuality', [], [PermissionKey.peAktuality, PermissionLevel.P_OWNED]],
    ['Nástěnka', '/admin/nastenka', [], [PermissionKey.peNastenka, PermissionLevel.P_OWNED]],
    ['Rozpis', '/admin/rozpis', [], [PermissionKey.peRozpis, PermissionLevel.P_OWNED]],
    ['Nabídka', '/admin/nabidka', [], [PermissionKey.peNabidka, PermissionLevel.P_OWNED]],
    ['Akce', '/admin/akce', [], [PermissionKey.peAkce, PermissionLevel.P_OWNED]],
    ['Galerie', '/admin/galerie', [], [PermissionKey.peGalerie, PermissionLevel.P_OWNED]],
    ['Video', '/admin/video', [], [PermissionKey.peAktuality, PermissionLevel.P_OWNED]],
    ['Dokumenty', '/admin/dokumenty', [], [PermissionKey.peDokumenty, PermissionLevel.P_OWNED]],
    ['Oprávnění', '/admin/permissions', [], [PermissionKey.pePermissions, PermissionLevel.P_OWNED]],
  ], [PermissionKey.peNastenka, PermissionLevel.P_OWNED]]
];

/* const NavbarItem: React.FC<{ pathname: string; item: NavbarItem; }> = ({ item, pathname }) => {
 *   const permissions = usePermissions();
 *
 *   if (item.length === 1) {
 *     return <div className={item[0]} />;
 *   }
 *   if (item[3] && !permissions.hasPermission(item[3][0], item[3][1])) {
 *     return null;
 *   }
 *   const active = (item[1] === pathname || pathname.includes(item[1])) ? ' active' : '';
 *
 *   if (!item[2]) {
 *     return <li className={`nav-item ${active}`}>
 *       <NextLinkComposed className="nav-link" href={item[1]}>
 *         {item[0]}
 *       </NextLinkComposed>
 *     </li>;
 *   }
 *
 *   return (
 *     <Dropdown className="nav-item">
 *       <Dropdown.Toggle className="nav-link" style={{ border: 'transparent', backgroundColor: 'transparent' }}>
 *         {item[0]}
 *       </Dropdown.Toggle>
 *       <Dropdown.Menu style={{ minWidth: '250px' }}>
 *         {item[2].map((sub: NavbarItem, i) => {
 *           if (sub[3] && !permissions.hasPermission(sub[3][0], sub[3][1])) {
 *             return null;
 *           }
 *           return <a key={i} className="dropdown-item" href={sub[1]}>{sub[0]}</a>;
 *         })}
 *       </Dropdown.Menu>
 *     </Dropdown>
 *   );
 * };
 */
