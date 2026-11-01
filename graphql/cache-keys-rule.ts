import { getNamedType, GraphQLError, isAbstractType, isObjectType, Kind } from 'graphql';
import type {
  FieldNode,
  GraphQLObjectType,
  SelectionSetNode,
  ValidationRule,
} from 'graphql';
import { cacheKeyFields } from '../frontend/lib/cache-keys.ts';

const keyFields = new Map<string, readonly string[] | null>(
  Object.entries(cacheKeyFields),
);

export const cacheKeyRule: ValidationRule = (context) => {
  const schema = context.getSchema();

  function selectedFields(
    selectionSet: SelectionSetNode,
    type: GraphQLObjectType,
    seen: ReadonlySet<SelectionSetNode> = new Set(),
    unconditional = true,
  ): { field: FieldNode; unconditional: boolean }[] {
    if (seen.has(selectionSet)) return [];
    const path = new Set([...seen, selectionSet]);
    return selectionSet.selections.flatMap((selection) => {
      let always = unconditional;
      for (const directive of selection.directives ?? []) {
        if (!['skip', 'include'].includes(directive.name.value)) continue;
        const value = directive.arguments?.find(
          (argument) => argument.name.value === 'if',
        )?.value;
        if (value?.kind !== Kind.BOOLEAN) always = false;
        else if (value.value !== (directive.name.value === 'include')) return [];
      }
      if (selection.kind === Kind.FIELD)
        return [{ field: selection, unconditional: always }];
      const fragment =
        selection.kind === Kind.FRAGMENT_SPREAD
          ? context.getFragment(selection.name.value)
          : selection;
      if (!fragment) return [];
      const condition =
        fragment.typeCondition && schema.getType(fragment.typeCondition.name.value);
      if (
        condition &&
        condition !== type &&
        !(isAbstractType(condition) && schema.isSubType(condition, type))
      )
        return [];
      return selectedFields(fragment.selectionSet, type, path, always);
    });
  }

  return {
    Field(node) {
      if (!node.selectionSet) return;
      const namedType = getNamedType(context.getType());
      const types = isAbstractType(namedType)
        ? schema.getPossibleTypes(namedType)
        : [namedType];
      for (const type of types) {
        if (!isObjectType(type)) continue;
        const configured = keyFields.get(type.name);
        const keys =
          configured === undefined ? (type.getFields().id ? ['id'] : null) : configured;
        if (!keys) continue;
        const fields = selectedFields(node.selectionSet, type);
        if (!fields.some(({ field }) => field.name.value !== '__typename')) continue;
        const missing = keys.filter(
          (key) =>
            !fields.some(
              ({ field, unconditional }) =>
                unconditional &&
                field.name.value === key &&
                (!field.alias || field.alias.value === key),
            ),
        );
        if (missing.length)
          context.reportError(
            new GraphQLError(
              `${type.name} must select unaliased, unconditional cache key fields: ${missing.join(', ')}.`,
              { nodes: node },
            ),
          );
      }
    },
  };
};
