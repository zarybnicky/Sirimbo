/**
 * @type {string}
 */
export const tenantId = process.env.NEXT_PUBLIC_TENANT_ID;

/**
 * @type {import("./types").Config}
 */
export const tenantConfig =
  parseInt(tenantId) === 1 ? await import('./olymp/config.mjs') :
  parseInt(tenantId) === 2 ? await import('./kometa/config.mjs') :
  undefined;

/**
 * @type {string}
 */
export const tenantAlias =
  parseInt(tenantId) === 1 ? './tenant/olymp' :
  parseInt(tenantId) === 2 ? './tenant/kometa' :
  null;

if (!tenantConfig) {
  throw new Error('Invalid tenant configuration');
}
