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

type TenantUiModule =
  typeof import('./olymp/ui') &
  typeof import('./starlet/ui') &
  typeof import('./kometa/ui');

type TenantLogos = {
  primary: StaticImageData | string;
  onDark?: StaticImageData | string;
  icon?: StaticImageData | string;
};

type TenancyCatalogEntry = {
  id: number;
  name: string;
  hosts: string[];
  config: Config;
  logos: TenantLogos;
  loadUiModule: () => Promise<TenantUiModule>;
};

const tenancyCatalog: TenancyCatalogEntry[] = [
  {
    id: 1,
    name: olympConfig.shortName,
    hosts: ['olymp.rozpisovnik.cz', 'tkolymp.cz'],
    config: olympConfig,
    logos: {
      primary: olympLogo,
      icon: olympLogo,
    },
    loadUiModule: () => import('./olymp/ui'),
  },
  {
    id: 2,
    name: kometaConfig.shortName,
    hosts: ['dspkometa.rozpisovnik.cz'],
    config: kometaConfig,
    logos: {
      primary: kometaLogo,
      onDark: kometaLogoOnDark,
    },
    loadUiModule: () => import('./kometa/ui'),
  },
  {
    id: 3,
    name: starletConfig.shortName,
    hosts: ['tkstarlet.rozpisovnik.cz'],
    config: starletConfig,
    logos: {
      primary: starletLogo,
      icon: starletIcon,
    },
    loadUiModule: () => import('./starlet/ui'),
  },
  {
    id: 4,
    name: kometaConfig.shortName,
    hosts: ['dspkometa2.rozpisovnik.cz'],
    config: kometaConfig,
    logos: {
      primary: kometaLogo,
      onDark: kometaLogoOnDark,
    },
    loadUiModule: () => import('./kometa/ui'),
  },
];

const tenancyById = new Map<number, TenancyCatalogEntry>();
const tenancyByHost = new Map<string, TenancyCatalogEntry>();

for (const entry of tenancyCatalog) {
  tenancyById.set(entry.id, entry);
  for (const host of entry.hosts) {
    tenancyByHost.set(host.toLowerCase(), entry);
  }
}

const runtimeTenantId = Number.parseInt(process.env.NEXT_PUBLIC_TENANT_ID ?? '1', 10);

async function fetchTenantUi(): Promise<TenantUiModule> {
  const entry = tenancyById.get(runtimeTenantId);
  if (!entry) {
    return await tenancyCatalog[0]!.loadUiModule();
  }
  return entry.loadUiModule();
}

export async function getTenantUi<K extends keyof TenantUiModule>(
  key: K,
): Promise<{ default: TenantUiModule[K] }> {
  const ui = await fetchTenantUi();
  const component = ui[key];
  if (typeof component !== 'function') {
    throw new TypeError(`Tenant UI module is missing component: ${String(key)}`);
  }
  return { default: component as TenantUiModule[K] };
}
