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
  Number.parseInt(tenantId) === 4 ? require('./kometa/config.js') :
  Number.parseInt(tenantId) === 3 ? require('./starlet/config.js') :
  undefined;

if (!module.exports.tenantConfig) {
  throw new Error('Invalid tenant configuration');
}
