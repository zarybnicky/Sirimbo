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
                const res = await fetch(url, {
                  method: "POST",
                  headers: {
                    "Content-Type": "application/json",
                    ...(auth ? { Cookie: `auth=${auth}` } : {}),
                  },
                  body: JSON.stringify(data),
                });
                if (!res.ok) {
                  const text = await res.text().catch(() => "");
                  throw new Error(`POST ${url} failed: ${res.status} ${res.statusText}${text ? ` — ${text}` : ""}`);
                }

                return await res.json();
              },
            );
          },
        },
      },
    };
  }),
];

export default plugins;
