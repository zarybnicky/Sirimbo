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
    "@typescript-eslint/no-misused-promises": ["error", {
      "checksVoidReturn": {
        "arguments": false,
        "attributes": false
      }
    }],
    "@typescript-eslint/no-unsafe-assignment": "warn",
    "@typescript-eslint/no-unsafe-call": "warn",
    "@typescript-eslint/no-unsafe-return": "warn",
    "@typescript-eslint/no-floating-promises": "warn",
    "@typescript-eslint/no-unsafe-member-access": "warn",
  }
}