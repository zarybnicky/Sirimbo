const path = require('path');

module.exports = {
  extends: [
    "next/core-web-vitals",
    "plugin:tailwindcss/recommended",
    'plugin:@typescript-eslint/recommended',
    'plugin:@typescript-eslint/recommended-requiring-type-checking',
  ],
  parserOptions: {
    project: [path.join(__dirname, './tsconfig.json')],
  },
  settings: {
    tailwindcss: {
      config: path.join(__dirname, '../../apps/olymp/tailwind.config.js'),
      callees: ["classNames", "ctl", "cn"]
    }
  },
  rules: {
    "tailwindcss/classnames-order": ["off"],
    "import/no-unused-modules": ["off", {
      "missingExports": true,
      unusedExports: true,
      "ignoreExports": ["./Calendar.tsx"]
    }],
    "@typescript-eslint/no-misused-promises": ["error", {
      "checksVoidReturn": {
        "arguments": false,
        "attributes": false
      }
    }],
  }
}
