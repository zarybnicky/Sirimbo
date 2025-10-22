export type TenantConfig = {
  shortName: string;
  copyrightLine: string;
  favicon: string;
  enableHome: boolean;
  enableArticles: boolean;
  enableRegistration: boolean;
  enableStarletImport?: boolean;
  useTrainerInitials: boolean;
  lockEventsByDefault: boolean;
  facebookPixelId?: string;
}
