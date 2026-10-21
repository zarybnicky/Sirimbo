import { TYPES, listOfCodec, type PgSelectQueryBuilder } from 'postgraphile/@dataplan/pg';
import { lambda } from 'postgraphile/grafast';
import { orderByAscDesc } from 'postgraphile/utils';

const progressCodec = listOfCodec(TYPES.numeric);
type CategoryOrder = { discipline: string; descending: boolean };

const plugin: GraphileConfig.Plugin = {
  name: 'FederationOrderPlugin',
  after: ['PgOrderAllAttributesPlugin'],
  before: ['PgConnectionArgOrderByPlugin'],
  schema: {
    hooks: {
      GraphQLEnumType_values(values, { sql }, { scope }) {
        const table = scope.pgCodec?.extensions?.pg;
        if (
          !scope.isPgRowSortEnum ||
          table?.schemaName !== 'federated' ||
          table.name !== 'competitor'
        )
          return values;
        return {
          ...values,
          ...orderByAscDesc(
            'NAME',
            (query) => ({
              fragment: sql`nullif(trim(${query.alias}.name), '')`,
              codec: TYPES.text,
            }),
            { nulls: 'last' },
          ),
        };
      },
      init(_, build) {
        build.registerInputObjectType(
          'CompetitorCategoryOrder',
          {},
          () => ({
            fields: {
              discipline: {
                type: new build.graphql.GraphQLNonNull(build.graphql.GraphQLString),
              },
              descending: {
                type: new build.graphql.GraphQLNonNull(build.graphql.GraphQLBoolean),
                defaultValue: true,
              },
            },
          }),
          'FederationOrderPlugin',
        );
        return _;
      },
      GraphQLObjectType_fields_field_args(args, build, { scope }) {
        const {
          isPgFieldConnection,
          isPgFieldSimpleCollection,
          pgFieldResource,
          pgFieldCodec,
        } = scope;
        const table = (pgFieldCodec ?? pgFieldResource?.codec)?.extensions?.pg;
        if (
          (!isPgFieldConnection && !isPgFieldSimpleCollection) ||
          table?.schemaName !== 'federated' ||
          table.name !== 'competitor'
        )
          return args;
        const { sql } = build;
        return build.extend(
          args,
          {
            orderByCategory: {
              type: build.getInputTypeByName('CompetitorCategoryOrder'),
              applyPlan(_, $field, arg) {
                const $select = isPgFieldConnection ? $field.getSubplan() : $field;
                $select.apply(
                  lambda(arg.getRaw(), (value: CategoryOrder | null) => {
                    if (!value) return null;
                    return (query: PgSelectQueryBuilder) => {
                      query.orderBy({
                        fragment: sql`coalesce((
                          select array[
                            coalesce(array_position(
                              array['Entry', 'Novice', 'Bronze', 'Silver', 'Gold', 'E', 'D', 'C', 'B', 'A', 'M'],
                              case cat.class when 'S' then 'M' else cat.class end
                            ), 0),
                            p.domestic_finals + p.foreign_finals,
                            p.points
                          ]
                          from federated.competitor_category_progress p
                          join federated.category cat on cat.id = p.category_id
                          where p.competitor_id = ${query.alias}.id
                            and cat.discipline = ${sql.value(value.discipline)}
                          order by 1 desc
                          limit 1
                        ), array[${sql.value(value.descending ? '-Infinity' : 'Infinity')}::numeric])`,
                        codec: progressCodec,
                        direction: value.descending ? 'DESC' : 'ASC',
                        nullable: false,
                      });
                      query.orderBy({ attribute: 'name', direction: 'ASC' });
                      query.orderBy({ attribute: 'external_id', direction: 'ASC' });
                    };
                  }),
                );
              },
            },
          },
          'FederationOrderPlugin',
        );
      },
    },
  },
};

export default plugin;
