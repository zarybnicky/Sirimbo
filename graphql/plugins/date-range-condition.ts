import { extendSchema } from 'postgraphile/utils';

export const DateRangeConditionPlugin = extendSchema({
  typeDefs: `
    input DateRangeCondition {
      """Inclusive start date."""
      since: Date
      """Exclusive end date."""
      until: Date
    }
  `,
});
