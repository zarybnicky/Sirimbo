import { useAuth, useTenantConfig } from '@/lib/auth';
import { canAccess, type AccessRequirements, type ResolvedAuth } from '@/lib/auth-claims';
import type { TenantConfig } from '@/tenant/types';

export type MenuLink = AccessRequirements & {
  type: 'link';
  title: string;
  href: string;
  className?: string;
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export function getHrefs(x: MenuStructItem): string[] {
  return x.type === 'link' ? [x.href] : x.children.flatMap((x) => getHrefs(x));
}

function filterMenu(
  items: readonly MenuStructItem[],
  auth: ResolvedAuth,
  tenant: TenantConfig,
): MenuStructItem[] {
  const result: MenuStructItem[] = [];
  for (const item of items) {
    if (item.type === 'link') {
      if (canAccess(auth, tenant, item)) result.push(item);
    } else {
      const children = item.children.filter((x) => canAccess(auth, tenant, x));
      if (children.length > 0) result.push({ ...item, children });
    }
  }
  return result;
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
      {
        type: 'link',
        title: 'Přípravka tanečního sportu',
        href: 'https://nabor.tkolymp.cz',
      },
      { type: 'link', title: 'Vystoupení na akcích', href: '/vystoupeni' },
      { type: 'link', title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
    ],
  },
  { type: 'link', title: 'Aktuality', href: '/clanky' },
  { type: 'link', title: 'Galerie', href: '/galerie' },
  { type: 'link', title: 'Akce', href: '/akce' },
  { type: 'link', title: 'Kontakt', href: '/kontakt' },
];

const memberMenu: MenuStructItem[] = [
  {
    type: 'link',
    title: 'Nástěnka',
    href: '/dashboard?tab=myAnnouncements',
  },
  {
    type: 'link',
    title: 'Stálá nástěnka',
    className: 'lg:hidden',
    href: '/dashboard?tab=stickyAnnouncements',
    requirePublicSite: true,
  },
  { type: 'link', title: 'Profil', href: '/profil' },
  {
    type: 'menu',
    title: 'Tréninky',
    children: [
      {
        type: 'link',
        title: 'Moje tréninky',
        href: '/dashboard?tab=myLessons',
      },
      { type: 'link', title: 'Kalendář', href: '/rozpis' },
      { type: 'link', title: 'Seznam akcí', href: '/akce' },
    ],
  },
  {
    type: 'menu',
    title: 'Taneční klub',
    children: [
      { type: 'link', title: 'Klub', href: '/tanecni-klub' },
      { type: 'link', title: 'Tréninkové skupiny', href: '/treninkove-skupiny' },
      { type: 'link', title: 'Páry', href: '/pary' },
      { type: 'link', title: 'Členové', href: '/clenove' },
      { type: 'link', title: 'Žebříček', href: '/zebricek' },
    ],
  },
  {
    type: 'menu',
    title: 'Správa',
    children: [
      { type: 'link', title: 'Nástěnka', href: '/nastenka', requireTrainer: true },
      { type: 'link', title: 'Platby', href: '/platby', requireAdmin: true },
      {
        type: 'link',
        title: 'Články',
        href: '/aktuality',
        requireTrainer: true,
        requirePublicSite: true,
      },
      {
        type: 'link',
        title: 'Vyplněné formuláře',
        href: '/crm',
        requireAdmin: true,
        requirePublicSite: true,
      },
      {
        type: 'link',
        title: 'Přístupy',
        href: '/pristupy',
        requireAdmin: true,
      },
      {
        type: 'link',
        title: 'Import z evidence',
        href: '/starlet-import',
        requireAdmin: true,
        requireStarletImport: true,
      },
    ],
  },
  {
    type: 'menu',
    title: 'Systém',
    children: [
      {
        type: 'link',
        title: 'Tenanti',
        href: '/admin/tenants',
        requireSystemAdmin: true,
      },
    ],
  },
];

export function useMemberMenu(): MenuStructItem[] {
  const auth = useAuth();
  const tenant = useTenantConfig();
  return filterMenu(memberMenu, auth, tenant);
}
