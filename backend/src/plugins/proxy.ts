import { gql, makeExtendSchemaPlugin } from 'postgraphile/utils';
import { withPgClient } from 'postgraphile/@dataplan/pg';
import { ExecutableStep } from 'postgraphile/grafast';

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
        evidenceStarlet(_$parent, { $url, $data, $auth }, _info) {
          return withPgClient(executor, { $url, $data, $auth } as unknown as ExecutableStep<any>, async (client, { url, data, auth }: any) => {
            if (auth) {
              const { rows: [response] } = await client.query({
                text: "select content from post_without_cache($1, array[http_header('Authorization', $2)], data)",
                values: [url, auth, data],
              });
              return JSON.parse((response as any).content) as any;
            } else {
              const { rows: [response] } = await client.query({
                text: "select content from post_without_cache($1, array[], $2)",
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
