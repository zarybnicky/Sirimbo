import { TenantConfig } from '../types';

export const config: TenantConfig = {
  origin: 'tkstarletbrno.rozpisovnik.cz',
  shortName: 'TK Starlet',
  copyrightLine: '© 2024 TK Starlet Brno, z. s.',
  favicon: '',
  seo: {
    titleTemplate: '%s · TK Starlet',
    defaultTitle: 'TK Starlet',
    themeColor: '#000',
    openGraph: { siteName: 'TK Starlet' },
    additionalMetaTags: [
      { name: 'viewport', content: 'initial-scale=1,width=device-width' },
    ],
  },
  enableRegistration: false,
  enableStarletImport: true,
  useTrainerInitials: true,
  lockEventsByDefault: true,
};
