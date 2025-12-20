import { gql, extendSchema } from 'postgraphile/utils';
import { withPgClient } from 'postgraphile/@dataplan/pg';
import { object } from 'postgraphile/grafast';

const plugins: GraphileConfig.Plugin[] = [
  extendSchema((build) => {
    const executor = build.input.pgRegistry.pgExecutors.main;

    return {
      typeDefs: gql`
        extend type Query {
          evidenceStarlet(url: String!, data: JSON!, auth: String): JSON
        }
      `,
      plans: {
        Query: {
          evidenceStarlet(_$parent: any, args: any) {
            const $args = object({
              url: args.getRaw('url'),
              data: args.getRaw('data'),
              auth: args.getRaw('auth'),
            });
            return withPgClient(
              executor,
              $args,
              async function (client, { url, auth, data }: any) {
                if (auth) {
                  const {
                    rows: [response],
                  } = await client.query({
                    text: "select content from http('POST', $1, array[http_header('Cookie', 'auth=' || $2)], 'application/json', $3::text)",
                    values: [url, auth, data],
                  });
                  return JSON.parse((response as any).content) as any;
                } else {
                  const {
                    rows: [response],
                  } = await client.query({
                    text: "select content from http('POST', $1, array[]::http_header[], 'application/json', $2::text)",
                    values: [url, data],
                  });
                  return JSON.parse((response as any).content) as any;
                }
              },
            );
          },
        },
      },
    };
  }),
];

export default plugins;
