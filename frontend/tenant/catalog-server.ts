import type { TenantConfig } from './types';

import { config as kometaConfig } from './kometa/config';
import { config as olympConfig } from './olymp/config';
import { config as starletConfig } from './starlet/config';

export type ServerTenantCatalogEntry = {
  id: number;
  name: string;
  hosts: string[];
  config: TenantConfig;
};

export const serverTenantCatalog = {
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

const serverTenants = Object.values(serverTenantCatalog);

export function parseTenant(
  tenantId: string | number | null | undefined,
): ServerTenantCatalogEntry | undefined {
  const id = Number.parseInt(String(tenantId));
  return serverTenants.find((entry) => entry.id === id);
}

export function getServerTenant(tenantId: string | number): ServerTenantCatalogEntry {
  return parseTenant(tenantId) ?? serverTenantCatalog[1];
}

export const hostToTenant = new Map<string, ServerTenantCatalogEntry>();

for (const entry of serverTenants) {
  for (const host of entry.hosts) {
    hostToTenant.set(host.toLowerCase(), entry);
  }
}
