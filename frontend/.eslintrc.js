const path = require('node:path');

module.exports = {
  extends: [
    "next/core-web-vitals",
    "eslint:recommended",
    "plugin:tailwindcss/recommended",
    "plugin:unicorn/recommended",
    "plugin:@typescript-eslint/recommended",
    'plugin:de-morgan/recommended-legacy',
  ],
  settings: {
    tailwindcss: {
      config: path.join(__dirname, './tailwind.config.js'),
      callees: ["classNames", "ctl", "cn"],
      cssFilesRefreshRate: 5_000_000,
    }
  },
  rules: {
    "no-extra-semi": "off",
    "no-irregular-whitespace": "off",
    "no-unused-vars": "off",
    "prefer-const": "warn",
    "@typescript-eslint/no-unused-vars": ["warn", {
      "args": "all",
      "argsIgnorePattern": "^_",
      "caughtErrors": "all",
      "caughtErrorsIgnorePattern": "^_",
      "destructuredArrayIgnorePattern": "^_",
      "varsIgnorePattern": "^_",
      "ignoreRestSiblings": true
    }],
    "react/function-component-definition": ["error", {
        "namedComponents": "function-declaration",
        "unnamedComponents": "arrow-function"
      }
    ],
    "@typescript-eslint/no-var-requires": "off",
    "@typescript-eslint/no-explicit-any": "off",
    "@typescript-eslint/no-non-null-asserted-optional-chain": "off",
    "@typescript-eslint/no-require-imports": "off",
    "tailwindcss/classnames-order": "off",
    "import/no-unused-modules": ["warn", {
      unusedExports: true,
      ignoreExports: ["pages/**"]
    }],
    "unicorn/prevent-abbreviations": "off",
    "unicorn/no-null": "off",
    "import/named": "off",
    "import/namespace": "off",
    "import/default": "off",
  },
}
