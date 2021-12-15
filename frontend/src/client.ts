import { HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';

export const createClient = () => new ApolloClient({
  link: new HttpLink({ uri: '/graphql/v1/graphql' }),
  cache: new InMemoryCache(),
});
