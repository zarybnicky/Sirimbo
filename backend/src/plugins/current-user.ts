import { extendSchema } from "postgraphile/utils";

const GetCurrentUserPlugin = extendSchema((build) => {
  const getCurrentUserFn = build.input.pgRegistry.pgResources.get_current_user;
  const string = build.input.pgRegistry.pgCodecs.string;

  return {
    typeDefs: `
      extend type Query {
        getCurrentUser(versionId: String): User
      }
    `,
    objects: {
      Query: {
        plans: {
          getCurrentUser(_parent, { $versionId }, _context) {
            // call the SQL function via pgResource
            return getCurrentUserFn.execute([{ step: $versionId, pgCodec: string, name: 'version_id' }]);
          },
        },
      },
    },
  };
});

export default GetCurrentUserPlugin;
