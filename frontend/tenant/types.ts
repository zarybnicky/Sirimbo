type TenantMetaTag = {
  name: string;
  content: string;
};

type TenantLinkTag = {
  rel: string;
  href: string;
  sizes?: string;
  color?: string;
};

export type TenantSeoConfig = {
  titleTemplate: string;
  defaultTitle: string;
  themeColor: string;
  facebook?: {
    appId: string;
  };
  openGraph: {
    siteName: string;
  };
  additionalMetaTags: TenantMetaTag[];
  additionalLinkTags?: TenantLinkTag[];
};

export type TenantConfig = {
  shortName: string;
  copyrightLine: string;
  favicon: string;
  seo: TenantSeoConfig;
  enableHome: boolean;
  enableRegistration: boolean;
  enableStarletImport?: boolean;
  useTrainerInitials: boolean;
  lockEventsByDefault: boolean;
  facebookPixelId?: string;
};
