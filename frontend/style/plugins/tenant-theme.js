const { tenantConfig: tenant } = require("../../tenant/config.js");

module.exports = function tenantThemePlugin({ addBase }) {
  addBase({
    ":root": {
      "--tenant-primary": tenant.themePrimary,
    },
  });
};
