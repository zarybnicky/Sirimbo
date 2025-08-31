import { gql, makeExtendSchemaPlugin } from 'postgraphile/utils';
import { withPgClient } from 'postgraphile/@dataplan/pg';
import { object } from 'postgraphile/grafast';

const plugins: GraphileConfig.Plugin[] = [
  makeExtendSchemaPlugin((build) => {
    const executor = build.input.pgRegistry.pgExecutors.main;

    return {
    typeDefs: gql`
      extend type Query {
        cstsAthlete(idt: Int!): JSON
        wdsfAthlete(min: Int!): JSON
        evidenceStarlet(url: String!, data: JSON!, auth: String): JSON
      }
    `,
    plans: {
      Query: {
        cstsAthlete(_$parent, args, _info) {
          return withPgClient(executor, args.get('idt'), async (client, idt) => {
            const { rows: [response] } = await client.query({
              text: "select content from fetch_with_cache('https://www.csts.cz/api/1/athletes/' || $1, array[http_header('Referrer', 'https://www.csts.cz')])",
              values: [idt],
            });
            return JSON.parse((response as any).content).collection[0];
          });
        },
        wdsfAthlete(_$parent, args, _info) {
          return withPgClient(executor, args.get('min'), async (client, min) => {
            const url = `https://services.worlddancesport.org/api/1/person/${min}?format=json`;
            const { rows: [response] } = await client.query({
              text: "select content from fetch_with_cache($1, array[http_header('Authorization', $2)])",
              values: [url, process.env.WDSF_AUTH || ''],
            });
            return JSON.parse((response as any).content) as any;
          });
        },
        evidenceStarlet(_$parent, args, _info) {
          const $args = object({
            url: args.get('url'),
            data: args.get('data'),
            auth: args.get('auth'),
          });
          return withPgClient(executor, $args, async function (client, { url, data, auth }: any) {
            if (auth) {
              const { rows: [response] } = await client.query({
                text: "select content from post_without_cache($1, $2, array[http_header('Cookie', 'auth=' || $3)])",
                values: [url, data, auth],
              });
              return JSON.parse((response as any).content) as any;
            } else {
              const { rows: [response] } = await client.query({
                text: "select content from post_without_cache($1, $2, array[]::http_header[])",
                values: [url, data],
              });
              return JSON.parse((response as any).content) as any;
            }
          });
        },
      },
    },
  };
  }),
];

export default plugins;
