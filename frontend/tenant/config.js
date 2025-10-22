const tenantId = Number.parseInt(process.env.NEXT_PUBLIC_TENANT_ID || '1');

/**
 * @type {import("./types").Config}
 */
module.exports.tenantConfig =
  tenantId === 1 ? require('./olymp/config.js') :
  tenantId === 2 ? require('./kometa/config.js') :
  tenantId === 4 ? require('./kometa/config.js') :
  tenantId === 3 ? require('./starlet/config.js') :
  undefined;

if (!module.exports.tenantConfig) {
  throw new Error('Invalid tenant configuration');
}
