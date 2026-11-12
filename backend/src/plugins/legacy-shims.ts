import assert from 'node:assert';
import { sql, TYPES } from 'postgraphile/@dataplan/pg';
import { applyInput, lambda } from 'postgraphile/grafast';
import { extendSchema } from 'postgraphile/utils';

export const LegacyShimsPlugin = extendSchema((build) => ({
  typeDefs: `
    enum EventInstanceRangeScope {
      ALL
      MINE
      RELEVANT
    }

    extend type Query {
      eventInstancesForRangeList(
        onlyType: EventType
        startRange: Datetime
        endRange: Datetime
        trainerIds: [BigInt]
        participantIds: [BigInt]
        parentId: BigInt
        scope: EventInstanceRangeScope = ALL
        locationIds: [BigInt]
        onlyTypes: [EventType]
        first: Int
        offset: Int
      ): [Event!] @deprecated(reason: "Use eventsList with filters.")
    }
  `,
  objects: {
    Query: {
      plans: {
        eventInstancesForRangeList(_root, args) {
          const $events = build.input.pgRegistry.pgResources.event.find();
          const $condition = lambda(args.getRaw(), (input) => ({
            type: input.onlyType ?? undefined,
            types: input.onlyTypes,
            trainerIds: input.trainerIds,
            participant: input.participantIds == null ? undefined : { personIds: input.participantIds },
            locationIds: input.locationIds,
            parentId: input.parentId ?? undefined,
            onlyMine: input.scope === 'MINE' ||
              (input.scope === null && input.parentId != null),
            relevantOnly: input.scope === 'RELEVANT' && input.parentId == null,
          }));
          // Grafast accepts computed input steps; its input type excludes LambdaStep.
          $events.apply(applyInput(
            build.getInputTypeByName('EventCondition'),
            $condition as unknown as Parameters<typeof applyInput>[1],
            (qb) => qb.whereBuilder(),
          ));

          // Preserve the old function's tenant restriction and range semantics,
          // including omitted/null bounds and zero-length or reversed windows.
          const row = $events.alias;
          const start = $events.placeholder(args.$startRange, TYPES.timestamptz);
          const end = $events.placeholder(args.$endRange, TYPES.timestamptz);
          const scope = $events.placeholder(args.$scope, TYPES.text);
          const parent = $events.placeholder(args.$parentId, TYPES.bigint);
          $events.where(sql`${row}.tenant_id = public.current_tenant_id()`);
          $events.where(sql`(${scope} is not null or ${parent} is not null)`);
          $events.where(sql`${start} is not null
            and ${row}.since < coalesce(${end}, 'infinity'::timestamptz)
            and app_private.tstzrange_overlaps(${row}.range, tstzrange(
              ${start}, greatest(${start}, coalesce(${end}, 'infinity'::timestamptz)), '[]'))`);
          $events.orderBy({ attribute: 'since', direction: 'ASC' });
          $events.setFirst(args.$first);
          $events.setOffset(args.$offset);
          return $events;
        },
      },
    },
  },
}));

const legacyFields: Record<string, Record<string, string>> = {
  // Map old field names to current fields. Aliases are deprecated automatically.
  // Query: { eventInstance: 'event' },
};

export const LegacyRelationAliasesPlugin: GraphileConfig.Plugin = {
  name: 'LegacyRelationAliasesPlugin',
  schema: {
    hooks: {
      GraphQLObjectType_fields: {
        after: ['PgRelationsPlugin', 'PgMutationUpdateDeletePlugin', 'PgCustomTypeFieldPlugin', 'PersonCouplesPlugin'],
        callback(fields, build, { Self }) {
          const { GraphQLInputObjectType, GraphQLNonNull, getNamedType, isInputObjectType } =
            build.graphql;
          for (const [legacy, canonical] of Object.entries(legacyFields[Self.name] ?? {})) {
            if (!fields[canonical]) continue;
            const field = fields[canonical];
            let args = field.args;
            if (Self.name === 'Mutation') {
              const input = getNamedType(args!.input.type);
              assert(isInputObjectType(input));
              // Keep variable declarations using the old input name, including input-field plans.
              args = {
                ...args,
                input: {
                  ...args!.input,
                  type: new GraphQLNonNull(
                    new GraphQLInputObjectType({
                      ...input.toConfig(),
                      name: `${build.inflection.upperCamelCase(legacy)}Input`,
                    }),
                  ),
                },
              };
            }
            fields = {
              ...fields,
              [legacy]: {
                ...field,
                args,
                deprecationReason: `Use ${canonical}.`,
              },
            };
          }
          return fields;
        },
      },
    },
  },
};
