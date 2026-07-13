import type { TenantConfig } from './types';

import { config as kometaConfig } from './kometa/config';
import { config as olympConfig } from './olymp/config';
import { config as starletConfig } from './starlet/config';

export type TenantCatalogEntry = {
  id: number;
  name: string;
  hosts: string[];
  config: TenantConfig;
};

export const tenantCatalog: Record<number, TenantCatalogEntry> = {
  1: {
    id: 1,
    name: olympConfig.shortName,
    hosts: ['olymp.rozpisovnik.cz', 'tkolymp.cz'],
    config: olympConfig,
  },
  2: {
    id: 2,
    name: kometaConfig.shortName,
    hosts: ['dspkometa.rozpisovnik.cz'],
    config: kometaConfig,
  },
  3: {
    id: 3,
    name: starletConfig.shortName,
    hosts: ['tkstarletbrno.rozpisovnik.cz'],
    config: starletConfig,
  },
  4: {
    id: 4,
    name: 'DSP Kometa, kurzy',
    hosts: ['dspkometa2.rozpisovnik.cz'],
    config: kometaConfig,
  },
};

export const defaultTenant = tenantCatalog[2]!;

export function parseTenant(
  tenantId: string | number | null | undefined,
): TenantCatalogEntry | undefined {
  return tenantCatalog[Number.parseInt(String(tenantId))];
}

export function getServerTenant(tenantId: string | number): TenantCatalogEntry {
  return parseTenant(tenantId) ?? defaultTenant;
}

export const hostToTenant = new Map<string, TenantCatalogEntry>();
for (const entry of Object.values(tenantCatalog)) {
  for (const host of entry.hosts) {
    hostToTenant.set(host.toLowerCase(), entry);
  }
}
