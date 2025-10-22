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
    hosts: ['tkstarlet.rozpisovnik.cz'],
    config: starletConfig,
  },
  4: {
    id: 4,
    name: kometaConfig.shortName,
    hosts: ['dspkometa2.rozpisovnik.cz'],
    config: kometaConfig,
  },
};
