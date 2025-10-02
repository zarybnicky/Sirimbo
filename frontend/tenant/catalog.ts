import type { StaticImageData } from 'next/image';
import type { Config } from './types';

import olympConfig from './olymp/config.js';
import kometaConfig from './kometa/config.js';
import starletConfig from './starlet/config.js';

import olympLogo from './olymp/logo.webp';
import kometaLogo from './kometa/logo.webp';
import kometaLogoOnDark from './kometa/logo-white.webp';
import starletLogo from './starlet/starlet-logo.svg';
import starletIcon from './starlet/logo-white-no-text.png';

export type TenantSlug = 'olymp' | 'kometa' | 'starlet';

export type TenantUiModule = typeof import('./olymp/ui');

export type TenantLogos = {
  primary: StaticImageData | string;
  onDark?: StaticImageData | string;
  icon?: StaticImageData | string;
};

export type TenancyCatalogEntry = {
  id: number;
  slug: TenantSlug;
  name: string;
  hosts: string[];
  config: Config;
  logos: TenantLogos;
  loadUi: () => Promise<TenantUiModule>;
};

export const tenancyCatalog: TenancyCatalogEntry[] = [
  {
    id: 1,
    slug: 'olymp',
    name: olympConfig.shortName,
    hosts: ['olymp.rozpisovnik.cz', 'rozpisovnik.cz'],
    config: olympConfig,
    logos: {
      primary: olympLogo,
      icon: olympLogo,
    },
    loadUi: () => import('./olymp/ui'),
  },
  {
    id: 2,
    slug: 'kometa',
    name: kometaConfig.shortName,
    hosts: ['kometa.rozpisovnik.cz'],
    config: kometaConfig,
    logos: {
      primary: kometaLogo,
      onDark: kometaLogoOnDark,
    },
    loadUi: () => import('./kometa/ui'),
  },
  {
    id: 3,
    slug: 'starlet',
    name: starletConfig.shortName,
    hosts: ['starlet.rozpisovnik.cz'],
    config: starletConfig,
    logos: {
      primary: starletLogo,
      icon: starletIcon,
    },
    loadUi: () => import('./starlet/ui'),
  },
];

export const tenancyById = new Map<number, TenancyCatalogEntry>();
export const tenancyByHost = new Map<string, TenancyCatalogEntry>();
export const tenancyBySlug = new Map<TenantSlug, TenancyCatalogEntry>();

for (const entry of tenancyCatalog) {
  tenancyById.set(entry.id, entry);
  tenancyBySlug.set(entry.slug, entry);
  for (const host of entry.hosts) {
    tenancyByHost.set(host.toLowerCase(), entry);
  }
}

// Historical tenant mapping uses ID 4 for Kometa as well.
tenancyById.set(4, tenancyBySlug.get('kometa')!);

const FALLBACK_TENANT_ID = Number.parseInt(process.env.NEXT_PUBLIC_TENANT_ID ?? '1', 10);

export function getActiveTenantId(): number {
  return FALLBACK_TENANT_ID;
}

export function getTenantCatalogEntryById(id: number): TenancyCatalogEntry | undefined {
  return tenancyById.get(id);
}

export function getActiveTenantCatalogEntry(): TenancyCatalogEntry {
  return getTenantCatalogEntryById(FALLBACK_TENANT_ID) ?? tenancyCatalog[0];
}

export function loadTenantUiModuleBySlug(slug: TenantSlug): Promise<TenantUiModule> {
  const entry = tenancyBySlug.get(slug);
  if (!entry) {
    return Promise.reject(new Error(`Unknown tenant slug: ${slug}`));
  }
  return entry.loadUi();
}

export function loadTenantUiModuleById(id: number): Promise<TenantUiModule> {
  const entry = getTenantCatalogEntryById(id);
  if (!entry) {
    return Promise.reject(new Error(`Unknown tenant id: ${id}`));
  }
  return entry.loadUi();
}

export function loadActiveTenantUiModule(): Promise<TenantUiModule> {
  return loadTenantUiModuleById(FALLBACK_TENANT_ID).catch(() =>
    tenancyCatalog[0].loadUi()
  );
}
