import type { LinkProps } from 'next/link';

export type Route = LinkProps['href'];

export type NavigationLink = {
  title: string;
  href: Route;
  requireTrainer?: boolean;
  requireAdmin?: boolean;
  className?: string;
  recruitmentLink?: boolean;
};

export type NavigationMenu = {
  title: string;
  children: NavigationLink[];
};

export type NavigationItem = NavigationLink | NavigationMenu;

export type Config = {
  shortName: string;
  copyrightLine: string;
  favicon: string;
  enableHome: boolean;
  enableArticles: boolean;
  enableStarletImport?: boolean;
  useTrainerInitials: boolean;
  lockEventsByDefault: boolean;
  themePrimary: string;
  themeAccent: string;
  themeNeutral: string;
  facebookPixelId?: string;
  accentLight?: Record<string, string>;
  accentDark?: Record<string, string>;
  neutralLight?: Record<string, string>;
  neutralDark?: Record<string, string>;
  publicNavigation: NavigationItem[];
  memberNavigation?: NavigationItem[];
  enableRecruitmentLink?: boolean;
}
