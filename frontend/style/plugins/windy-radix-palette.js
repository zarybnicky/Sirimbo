const radixColors = require("@radix-ui/colors");
const palette = require("windy-radix-palette");
const { tenantConfig: tenant } = require("../../tenant/config.js");

module.exports = palette({
  colors: {
    green: radixColors.green,
    greenDark: radixColors.greenDark,
    [tenant.themeNeutral]: tenant.neutralLight || radixColors[tenant.themeNeutral],
    [`${tenant.themeNeutral}Dark`]: tenant.neutralDark || radixColors[`${tenant.themeNeutral}Dark`],
    [tenant.themeAccent]: tenant.accentLight || radixColors[tenant.themeAccent],
    [`${tenant.themeAccent}Dark`]: tenant.accentDark || radixColors[`${tenant.themeAccent}Dark`],
  },
});
