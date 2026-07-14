import { TenantConfig } from '../types';

export const config: TenantConfig = {
  origin: 'https://dspkometa.rozpisovnik.cz',
  copyrightLine: '© 2026 DSP Kometa Brno, z. s.',
  seo: {
    titleTemplate: '%s · DSP Kometa Brno',
    defaultTitle: 'DSP Kometa Brno',
    themeColor: '#000',
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
  enableRegistration: true,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
};
