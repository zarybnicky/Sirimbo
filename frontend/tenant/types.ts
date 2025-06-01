export type Config = {
  shortName: string;
  copyrightLine: string;
  favicon: string;
  enableHome: boolean;
  enableArticles: boolean;
  themePrimary: string;
  themeAccent: string;
  themeNeutral: string;
  accentLight?: Record<string, string>;
  accentDark?: Record<string, string>;
  neutralLight?: Record<string, string>;
  neutralDark?: Record<string, string>;
}
