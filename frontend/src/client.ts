import { HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';

export const createClient = () => new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});
