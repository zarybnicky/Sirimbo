/**
 * @typedef {Object} TenantConfig
 * @property {string} shortName
 * @property {string} copyrightLine
 * @property {string} favicon
 * @property {boolean} enableHome
 * @property {boolean} enableArticles
 */

/** @type {TenantConfig} */
const olymp = {
  shortName: "TK Olymp",
  copyrightLine: "© 2023 TK Olymp Olomouc, z. s.",
  favicon: '',
  enableHome: true,
  enableArticles: true,
};

/** @type {TenantConfig} */
const kometa = {
  shortName: "DSP Kometa",
  copyrightLine: "© 2023 DSP Kometa Brno, z. s.",
  favicon: '',
  enableHome: false,
  enableArticles: false,
};

export const currentTenantId = process.env.NEXT_PUBLIC_TENANT_ID ?? '1';

const tenant =
  parseInt(currentTenantId) === 1 ? olymp :
  parseInt(currentTenantId) === 2 ? kometa :
  null;
if (!tenant) {
  throw new Error('Invalid tenant configuration');
}
export const currentTenant = tenant;
