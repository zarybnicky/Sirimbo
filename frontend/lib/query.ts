export const origin = typeof window === 'undefined' ? `http://localhost:${process.env.PORT || 3000}` : '';
export const endpointUrl = origin + '/graphql';
