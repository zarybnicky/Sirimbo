import { gql } from 'graphile-utils';
import { Plugin } from 'graphile-build';
import { makeExtendSchemaPlugin } from 'postgraphile';

const plugins: Plugin[] = [
  makeExtendSchemaPlugin((_build) => ({
    typeDefs: gql`
      extend type Person {
        name: String! @requires(columns: ["prefix_title", "first_name", "last_name", "suffix_title"])
      }
    `,
    resolvers: {
      Person: {
        name: ({ prefixTitle, firstName, lastName, suffixTitle }) => {
          return [prefixTitle, firstName, lastName].join(" ") + (suffixTitle ? `, ${suffixTitle}` : '');
        },
      },
    },
  })),
];

export default plugins;
