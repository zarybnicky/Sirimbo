/** @type {import('ts-jest').JestConfigWithTsJest} **/
module.exports = {
  testEnvironment: "node",
  transform: {
    "^.+.tsx?$": ["@swc/jest"],
  },
  testPathIgnorePatterns: [
    "/.next/"
  ],
};
