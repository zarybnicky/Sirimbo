/**
 * @type {string}
 */
const tenantId = process.env.NEXT_PUBLIC_TENANT_ID || '1';
module.exports.tenantId = tenantId;

/**
 * @type {import("./types").Config}
 */
module.exports.tenantConfig =
  Number.parseInt(tenantId) === 1 ? require('./olymp/config.js') :
  Number.parseInt(tenantId) === 2 ? require('./kometa/config.js') :
  Number.parseInt(tenantId) === 3 ? require('./starlet/config.js') :
  undefined;

/**
 * @type {string}
 */
module.exports.tenantAlias =
  Number.parseInt(tenantId) === 1 ? './olymp' :
  Number.parseInt(tenantId) === 2 ? './kometa' :
  Number.parseInt(tenantId) === 3 ? './starlet' :
  null;

if (!module.exports.tenantConfig) {
  throw new Error('Invalid tenant configuration');
}
