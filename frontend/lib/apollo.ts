import { HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { ZeusScalars } from "./zeus";

export const scalars = ZeusScalars({
  BigInt: {
    encode: (e: unknown) => (e as number).toString(),
    decode: (e: unknown) => parseInt(e as string, 10),
  },
  JSON: {
    encode: (e: unknown) => JSON.stringify(e),
    decode: (e: unknown) => JSON.parse(e as string),
  },
  Date: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toDateString(),
  },
  Datetime: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toISOString(),
  },
  Time: {
    decode: (e: unknown) => new Date(e as string),
    encode: (e: unknown) => (e as Date).toTimeString(),
  },
});

export const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});
