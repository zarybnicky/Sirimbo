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
    name: 'TK Olymp Olomouc',
    hosts: ['olymp.rozpisovnik.cz', 'tkolymp.cz'],
    config: olympConfig,
  },
  2: {
    id: 2,
    name: 'DSP Kometa Brno',
    hosts: ['dspkometa.rozpisovnik.cz'],
    config: kometaConfig,
  },
  3: {
    id: 3,
    name: 'TK Starlet Brno',
    hosts: ['tkstarletbrno.rozpisovnik.cz'],
    config: starletConfig,
  },
  4: {
    id: 4,
    name: 'DSP Kometa, kurzy',
    hosts: ['dspkometa2.rozpisovnik.cz'],
    config: {
      ...kometaConfig,
      origin: 'https://dspkometa2.rozpisovnik.cz',
    },
  },
};

export const defaultTenant = tenantCatalog[1]!;

export const getTenant = (id: string | number | null | undefined) =>
  tenantCatalog[Number.parseInt(String(id))];

export const hostToTenant = new Map<string, TenantCatalogEntry>();
for (const entry of Object.values(tenantCatalog)) {
  for (const host of entry.hosts) {
    hostToTenant.set(host.toLowerCase(), entry);
  }
}
