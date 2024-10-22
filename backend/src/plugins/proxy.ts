import { gql } from 'graphile-utils';
import { Plugin } from 'graphile-build';
import { makeExtendSchemaPlugin } from 'postgraphile';
import type { Client } from 'pg';

const plugins: Plugin[] = [
  makeExtendSchemaPlugin((_build) => ({
    typeDefs: gql`
      extend type Query {
        cstsAthlete(idt: Int!): JSON
        wdsfAthlete(min: Int!): JSON
      }
    `,
    resolvers: {
      Query: {
        async cstsAthlete(_parent, { idt }, { pgClient }: { pgClient: Client }, _info) {
          const url = `https://www.csts.cz/api/1/athletes/${idt}`;
          const { rows: [cached] } = await pgClient.query("select response from http_cache where url = $1", [url]);
          if (cached) return cached.response.collection[0];
          const response = await fetch(url).then(x => x.json());
          await pgClient.query("insert into http_cache (url, response, expires_at) values ($1, $2, now() + interval '1 day')", [url, response]);
          return (response as any).collection[0];
        },
        async wdsfAthlete(_parent, { min }, { pgClient }: { pgClient: Client }, _info) {
          const url = `https://services.worlddancesport.org/api/1/person/${min}?format=json`;
          const { rows: [cached] } = await pgClient.query("select response from http_cache where url = $1", [url]);
          if (cached) return cached.response;
          const response = await fetch(url, {
            headers: {
              Authorization: process.env.WDSF_AUTH || '',
            },
          }).then(x => x.json());
          await pgClient.query("insert into http_cache (url, response, expires_at) values ($1, $2, now() + interval '1 day')", [url, response]);
          return (response as any);
        },
      },
    },
  })),
];

export default plugins;
