import { TenantConfig } from '../types';

export const config: TenantConfig = {
  origin: 'https://tkstarletbrno.rozpisovnik.cz',
  copyrightLine: '© 2026 TK Starlet Brno, z. s.',
  seo: {
    titleTemplate: '%s · TK Starlet',
    defaultTitle: 'TK Starlet',
    themeColor: '#000',
    additionalLinkTags: [
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/starlet/apple-touch-icon.png?v=3',
      },
      { rel: 'icon', sizes: '96x96', href: '/starlet/favicon-96x96.png?v=3' },
      { rel: 'icon', href: '/starlet/favicon.svg?v=3' },
      { rel: 'shortcut icon', href: '/starlet/favicon.ico?v=3' },
      { rel: 'manifest', href: '/starlet/site.webmanifest?v=3' },
    ],
  },
  enableRegistration: false,
  enableStarletImport: true,
  useTrainerInitials: true,
  lockEventsByDefault: true,
};
