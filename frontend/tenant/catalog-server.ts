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

export function getServerTenant(tenantId: string | number): ServerTenantCatalogEntry {
  const id = Number.parseInt(String(tenantId));
  return (
    Object.values(serverTenantCatalog).find((entry) => entry.id === id) ??
    serverTenantCatalog[1]
  );
}

export const hostToTenantId = new Map<string, string>();

for (const entry of Object.values(serverTenantCatalog)) {
  for (const host of entry.hosts) {
    hostToTenantId.set(host.toLowerCase(), String(entry.id));
  }
}
