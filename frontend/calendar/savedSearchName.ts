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
  const types = condition.types?.filter((type) => type != null);
  const categories =
    types?.length === Object.keys(typeNames).length || !types
      ? ['Události']
      : [
          ...types.map((type) => typeNames[type]),
          condition.withCompetitions && 'Soutěže',
          condition.withBirthdays && 'Narozeniny',
        ].filter(Boolean);
  let title = condition.canRegister ? 'Volné lekce' : categories.join(', ') || 'Události';
  if (condition.onlyMine)
    title = `Moje ${title[0]!.toLocaleLowerCase('cs')}${title.slice(1)}`;
  return [
    title,
    ...(options.people?.nodes ?? []).flatMap((person) =>
      condition.trainerIds?.includes(person.id)
        ? [`${person.name} (výuka)`]
        : condition.participantIds?.includes(person.id)
          ? [person.name]
          : [],
    ),
    options.couplesList?.find((couple) => couple.id === condition.coupleId)?.name,
    options.tenant?.cohortsList?.find((cohort) => cohort.id === condition.cohortId)?.name,
    ...(options.tenant?.locationsList ?? []).flatMap((location) =>
      condition.locationIds?.includes(location.id) ? [location.name] : [],
    ),
    options.parents?.find((parent) => parent.id === condition.parentId)?.name,
    search.crossTenant && 'všechny kluby',
  ]
    .filter(Boolean)
    .join(' – ');
}
