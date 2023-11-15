const path = require('path');

module.exports = {
  extends: [
    "next/core-web-vitals",
    "plugin:tailwindcss/recommended",
  ],
  settings: {
    tailwindcss: {
      config: path.join(__dirname, './tailwind.config.js'),
      callees: ["classNames", "ctl", "cn"]
    }
  },
  rules: {
    "tailwindcss/classnames-order": ["off"],
    "import/no-unused-modules": ["off", {
      unusedExports: true,
      ignoreExports: ["pages/**"]
    }],
    "@typescript-eslint/no-non-null-assertion": "warn",
  }
}
