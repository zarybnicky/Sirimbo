import type { ComponentType } from 'react';

import {
  DesktopLogo as OlympDesktopLogo,
  Footer as OlympFooter,
  MobileLogo as OlympMobileLogo,
  SidebarLogo as OlympSidebarLogo,
  SocialIcons as OlympSocialIcons,
} from './olymp/ui';
import {
  MobileLogo as KometaMobileLogo,
  SidebarLogo as KometaSidebarLogo,
} from './kometa/ui';
import {
  MobileLogo as StarletMobileLogo,
  SidebarLogo as StarletSidebarLogo,
} from './starlet/ui';

type AppTenantUi = {
  DesktopLogo: ComponentType;
  MobileLogo: ComponentType;
  SidebarLogo: ComponentType;
  SocialIcons: ComponentType;
  Footer: ComponentType;
};

function Empty() {
  return null;
}

const appTenantUi: Record<number, AppTenantUi> = {
  1: {
    DesktopLogo: OlympDesktopLogo,
    MobileLogo: OlympMobileLogo,
    SidebarLogo: OlympSidebarLogo,
    SocialIcons: OlympSocialIcons,
    Footer: OlympFooter,
  },
  2: {
    DesktopLogo: Empty,
    MobileLogo: KometaMobileLogo,
    SidebarLogo: KometaSidebarLogo,
    SocialIcons: Empty,
    Footer: Empty,
  },
  3: {
    DesktopLogo: Empty,
    MobileLogo: StarletMobileLogo,
    SidebarLogo: StarletSidebarLogo,
    SocialIcons: Empty,
    Footer: Empty,
  },
  4: {
    DesktopLogo: Empty,
    MobileLogo: KometaMobileLogo,
    SidebarLogo: KometaSidebarLogo,
    SocialIcons: Empty,
    Footer: Empty,
  },
};

export function getAppTenantUi(tenantId: string | number) {
  return appTenantUi[Number.parseInt(String(tenantId))] ?? appTenantUi[1]!;
}
