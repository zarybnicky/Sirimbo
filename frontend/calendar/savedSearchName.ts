import type { EventSearchConditionInput } from '../graphql/index.ts';

type Named = { id: string; name: string };

type SearchNameOptions = {
  people?: { nodes: Named[] } | null;
  couplesList?: Named[] | null;
  tenant?: { cohortsList?: Named[]; locationsList?: Named[] } | null;
  parents?: { id: string; name: string | null }[] | null;
};

const typeNames = {
  LESSON: 'Lekce',
  GROUP: 'Společné tréninky',
  RESERVATION: 'Nabídky lekcí',
  CAMP: 'Soustředění',
  HOLIDAY: 'Prázdniny',
};

// U+2063 marks a stored automatic name without adding visible punctuation.
export const automaticSearchNamePrefix = '\u2063';

export function savedSearchName(name: string) {
  return name.startsWith(automaticSearchNamePrefix) ? name.slice(1) : name;
}

export function suggestSavedSearchName(
  search: {
    condition: EventSearchConditionInput;
    crossTenant?: boolean | null;
  },
  options: SearchNameOptions = {},
) {
  const { condition } = search;
  const types = condition.types?.filter((x) => x != null);
  const categories =
    types?.length === Object.keys(typeNames).length || !types
      ? ['Události']
      : [
          ...types.map((x) => typeNames[x]),
          condition.withCompetitions && 'Soutěže',
          condition.withBirthdays && 'Narozeniny',
        ].filter(Boolean);
  let title = condition.canRegister ? 'Volné lekce' : categories.join(', ') || 'Události';
  if (condition.onlyMine)
    title = `Moje ${title[0]!.toLocaleLowerCase('cs')}${title.slice(1)}`;
  return [
    title,
    ...(options.people?.nodes ?? []).flatMap(({ id, name }) =>
      condition.trainerIds?.includes(id)
        ? [`${name} (výuka)`]
        : condition.participantIds?.includes(id)
          ? [name]
          : [],
    ),
    options.couplesList?.find((x) => x.id === condition.coupleId)?.name,
    options.tenant?.cohortsList?.find((x) => x.id === condition.cohortId)?.name,
    ...(options.tenant?.locationsList ?? []).flatMap((x) =>
      condition.locationIds?.includes(x.id) ? [x.name] : [],
    ),
    options.parents?.find((x) => x.id === condition.parentId)?.name,
    search.crossTenant && 'všechny kluby',
  ]
    .filter(Boolean)
    .join(' – ');
}
