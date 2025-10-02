import type { ComponentType } from 'react';
import type { StaticImageData } from 'next/image';
import type { Config } from './types';

import kometaConfig from './kometa/config.js';
import olympConfig from './olymp/config.js';
import starletConfig from './starlet/config.js';

import kometaLogo from './kometa/logo.webp';
import kometaLogoOnDark from './kometa/logo-white.webp';
import olympLogo from './olymp/logo.webp';
import starletIcon from './starlet/logo-white-no-text.png';
import starletLogo from './starlet/starlet-logo.svg';

export type TenantSlug = 'olymp' | 'kometa' | 'starlet';

export type TenantUiModule = typeof import('./olymp/ui');

type TenantUiExports = TenantUiModule[keyof TenantUiModule];

export type TenantUiComponent = Extract<TenantUiExports, ComponentType<unknown>>;

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
  loadUiModule: () => Promise<TenantUiModule>;
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
    loadUiModule: () => import('./olymp/ui'),
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
    loadUiModule: () => import('./kometa/ui'),
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
    loadUiModule: () => import('./starlet/ui'),
  },
];

const resolvedDefaultTenant = tenancyCatalog[0];
if (!resolvedDefaultTenant) {
  throw new Error('Tenancy catalog must contain at least one tenant.');
}
const defaultTenant = resolvedDefaultTenant;

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

const runtimeTenantId = Number.parseInt(process.env.NEXT_PUBLIC_TENANT_ID ?? '1', 10);

export function getRuntimeTenantId(): number {
  return runtimeTenantId;
}

export function findTenantById(id: number): TenancyCatalogEntry | undefined {
  return tenancyById.get(id);
}

export function getRuntimeTenant(): TenancyCatalogEntry {
  return findTenantById(runtimeTenantId) ?? defaultTenant;
}

export function importTenantUiModuleBySlug(slug: TenantSlug): Promise<TenantUiModule> {
  const entry = tenancyBySlug.get(slug);
  if (!entry) {
    return Promise.reject(new Error(`Unknown tenant slug: ${slug}`));
  }
  return entry.loadUiModule();
}

export function importTenantUiModuleById(id: number): Promise<TenantUiModule> {
  const entry = findTenantById(id);
  if (!entry) {
    return Promise.reject(new Error(`Unknown tenant id: ${id}`));
  }
  return entry.loadUiModule();
}

export function importRuntimeTenantUiModule(): Promise<TenantUiModule> {
  return importTenantUiModuleById(runtimeTenantId).catch(() =>
    defaultTenant.loadUiModule()
  );
}

export async function importRuntimeTenantUiComponent<K extends keyof TenantUiModule>(
  key: K,
): Promise<{ default: TenantUiModule[K] }> {
  const module = await importRuntimeTenantUiModule();
  const component = module[key];
  if (typeof component !== 'function') {
    throw new Error(`Tenant UI module is missing component: ${String(key)}`);
  }
  return { default: component as TenantUiModule[K] };
}
