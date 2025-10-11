export const env = {
  GRAPHQL_BACKEND:
    process.env.EXPO_PUBLIC_GRAPHQL_BACKEND ?? 'http://localhost:3000',
  TENANT_ID: process.env.EXPO_PUBLIC_TENANT_ID ?? '1',
  VERSION_ID: process.env.EXPO_PUBLIC_MOBILE_VERSION ?? 'mobile-client',
};
