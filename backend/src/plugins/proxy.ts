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
          const { rows: [response] } = await pgClient.query(
            "select content from fetch_with_cache($1, array[http_header('Referrer', 'https://www.csts.cz')])",
            [url],
          );
          return JSON.parse(response.content).collection[0];
        },
        async wdsfAthlete(_parent, { min }, { pgClient }: { pgClient: Client }, _info) {
          const url = `https://services.worlddancesport.org/api/1/person/${min}?format=json`;
          const { rows: [response] } = await pgClient.query(
            "select content from fetch_with_cache($1, array[http_header('Authorization', $2)])",
            [url, process.env.WDSF_AUTH || '']
          );
          return JSON.parse(response.content) as any;
        },
      },
    },
  })),
];

export default plugins;
