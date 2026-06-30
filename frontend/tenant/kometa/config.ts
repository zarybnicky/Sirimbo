import { TenantConfig } from '../types';

export const config: TenantConfig = {
  shortName: 'DSP Kometa',
  copyrightLine: '© 2024 DSP Kometa Brno, z. s.',
  favicon: '',
  seo: {
    titleTemplate: '%s · DSP Kometa',
    defaultTitle: 'DSP Kometa',
    themeColor: '#000',
    openGraph: { siteName: 'DSP Kometa' },
    additionalMetaTags: [
      { name: 'viewport', content: 'initial-scale=1,width=device-width' },
    ],
    additionalLinkTags: [
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/kometa/apple-touch-icon.png?v=3',
      },
      { rel: 'icon', sizes: '32x32', href: '/kometa/favicon-32x32.png?v=3' },
      { rel: 'icon', sizes: '16x16', href: '/kometa/favicon-16x16.png?v=3' },
      { rel: 'shortcut icon', href: '/kometa/favicon.ico?v=3' },
      { rel: 'manifest', href: '/kometa/site.webmanifest?v=3' },
      {
        rel: 'mask-icon',
        color: '#da532c',
        href: '/kometa/safari-pinned-tab.svg?v=3',
      },
    ],
  },
  enableHome: false,
  enableRegistration: true,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
};
