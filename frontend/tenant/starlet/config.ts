import { TenantConfig } from '../types';

export const config: TenantConfig = {
  origin: 'https://tkstarletbrno.rozpisovnik.cz',
  copyrightLine: '© 2026 TK Starlet Brno, z. s.',
  seo: {
    titleTemplate: '%s · TK Starlet',
    defaultTitle: 'TK Starlet',
    themeColor: '#000',
  },
  enableRegistration: false,
  enableStarletImport: true,
  useTrainerInitials: true,
  lockEventsByDefault: true,
};
