/**
 * @type {string}
 */
const tenantId = process.env.NEXT_PUBLIC_TENANT_ID;
module.exports.tenantId = tenantId;

/**
 * @type {import("./types").Config}
 */
module.exports.tenantConfig =
  parseInt(tenantId) === 1 ? require('./olymp/config.js') :
  parseInt(tenantId) === 2 ? require('./kometa/config.js') :
  undefined;

/**
 * @type {string}
 */
module.exports.tenantAlias =
  parseInt(tenantId) === 1 ? './olymp' :
  parseInt(tenantId) === 2 ? './kometa' :
  null;

if (!module.exports.tenantConfig) {
  throw new Error('Invalid tenant configuration');
}
