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

type TenantOpenGraphImage = {
  url: string;
  width?: number;
  height?: number;
  alt?: string;
};

export type TenantSeoConfig = {
  titleTemplate: string;
  defaultTitle: string;
  description?: string;
  themeColor: string;
  facebook?: {
    appId: string;
  };
  openGraph: {
    siteName: string;
    locale?: string;
    type?: string;
    images?: TenantOpenGraphImage[];
  };
  additionalMetaTags: TenantMetaTag[];
  additionalLinkTags?: TenantLinkTag[];
};

export type TenantPublicSiteConfig = {
  locale: string;
  image: TenantOpenGraphImage;
  organization: {
    name: string;
    legalName: string;
    logo: string;
    email?: string;
    telephone?: string;
    sameAs?: string[];
    address?: {
      streetAddress: string;
      addressLocality: string;
      postalCode: string;
      addressCountry: string;
    };
  };
};

export type TenantConfig = {
  origin: string;
  shortName: string;
  copyrightLine: string;
  favicon: string;
  seo: TenantSeoConfig;
  publicSite?: TenantPublicSiteConfig;
  enableRegistration: boolean;
  enableStarletImport?: boolean;
  useTrainerInitials: boolean;
  lockEventsByDefault: boolean;
  facebookPixelId?: string;
};
