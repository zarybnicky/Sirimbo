import { tenantConfig } from '@/tenant/config';
import type {
  Config,
  NavigationItem,
  NavigationLink,
  NavigationMenu,
  Route,
} from '@/tenant/types';

export type MenuLink = {
  type: 'link';
  title: string;
  href: Route;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  className?: string;
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export function getHrefs(x: MenuStructItem): Route[] {
  return x.type === 'link' ? [x.href] : x.children.flatMap((x) => getHrefs(x));
}

function toMenuLink(link: NavigationLink, config: Config): MenuLink | null {
  if (link.recruitmentLink && config.enableRecruitmentLink === false) {
    return null;
  }
  return {
    type: 'link',
    title: link.title,
    href: link.href,
    requireTrainer: link.requireTrainer,
    requireAdmin: link.requireAdmin,
    className: link.className,
  } satisfies MenuLink;
}

function isMenuLink(link: MenuLink | null): link is MenuLink {
  return link !== null;
}

function toMenuItem(item: NavigationItem, config: Config): MenuStructItem | null {
  if ('children' in item) {
    const children = item.children
      .map((child) => toMenuLink(child, config))
      .filter(isMenuLink);
    if (children.length === 0) {
      return null;
    }
    return { type: 'menu', title: item.title, children } satisfies MenuStructItem;
  }
  return toMenuLink(item, config);
}

function isMenuStructItem(item: MenuStructItem | null): item is MenuStructItem {
  return item !== null;
}

function buildMenu(items: NavigationItem[] | undefined, config: Config): MenuStructItem[] {
  return (items ?? [])
    .map((item) => toMenuItem(item, config))
    .filter(isMenuStructItem);
}

function getDefaultMemberNavigation(config: Config): NavigationItem[] {
  const navigation: NavigationItem[] = [
    {
      title: 'Nástěnka',
      href: {
        pathname: '/dashboard',
        query: { tab: 'myAnnouncements' },
      },
    },
  ];

  if (config.enableArticles) {
    navigation.push({
      title: 'Stálá nástěnka',
      className: 'lg:hidden',
      href: {
        pathname: '/dashboard',
        query: { tab: 'stickyAnnouncements' },
      },
    });
  }

  navigation.push({ title: 'Profil', href: '/profil' });

  const trainingMenu: NavigationMenu = {
    title: 'Tréninky',
    children: [
      {
        title: 'Moje tréninky',
        href: {
          pathname: '/dashboard',
          query: { tab: 'myLessons' },
        },
      },
      { title: 'Kalendář', href: '/rozpis' },
      { title: 'Seznam akcí', href: '/akce' },
    ],
  };

  const clubMenu: NavigationMenu = {
    title: 'Taneční klub',
    children: [
      { title: 'Klub', href: '/tanecni-klub' },
      { title: 'Tréninkové skupiny', href: '/treninkove-skupiny' },
      { title: 'Páry', href: '/pary' },
      { title: 'Členové', href: '/clenove' },
      { title: 'Žebříček', href: '/zebricek' },
    ],
  };

  if (config.enableArticles) {
    clubMenu.children.push({ title: 'Dokumenty', href: '/dokumenty' });
  }

  const adminMenu: NavigationMenu = {
    title: 'Správa',
    children: [
      { title: 'Pozvánky', href: '/pozvanky', requireAdmin: true },
      { title: 'Nástěnka', href: '/nastenka', requireTrainer: true },
      { title: 'Platby', href: '/platby', requireAdmin: true },
    ],
  };

  if (config.enableArticles) {
    adminMenu.children.push(
      { title: 'Články', href: '/aktuality', requireTrainer: true },
      { title: 'Vyplněné formuláře', href: '/crm', requireAdmin: true },
      { title: 'Upload (WIP)', href: '/upload', requireAdmin: true },
    );
  }

  if (config.enableStarletImport) {
    adminMenu.children.push({
      title: 'Import z evidence',
      href: '/starlet-import',
      requireAdmin: true,
    });
  }

  navigation.push(trainingMenu, clubMenu, adminMenu);

  return navigation;
}

const defaultMemberNavigation = getDefaultMemberNavigation(tenantConfig);

export const topMenu: MenuStructItem[] = buildMenu(tenantConfig.publicNavigation, tenantConfig);

export const memberMenu: MenuStructItem[] = buildMenu(
  tenantConfig.memberNavigation ?? defaultMemberNavigation,
  tenantConfig,
);
