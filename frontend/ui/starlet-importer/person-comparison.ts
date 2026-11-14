import { compareText } from '../../lib/compare.ts';
import { slugify } from '../../lib/slugify.ts';

export type ComparisonPerson = {
  id: string;
  firstName: string;
  lastName: string;
  birthYear: number | null;
  birthDate?: string | null;
  gender?: string | null;
  email: string | null;
  phone: string | null;
  externalIds: string[];
  groups: string[];
  isFormer?: boolean;
  isMember?: boolean;
  conflicts?: string[];
  registrations?: ComparisonPerson[];
  course?: string;
  street?: string | null;
  orientationNumber?: string | null;
  city?: string | null;
  postalCode?: string | null;
  keepReason?: string;
  unmatchedAction?: string;
};

type PersonMatch = {
  source?: ComparisonPerson;
  database?: ComparisonPerson;
  reason?: string;
  actions: string[];
  needsReview: boolean;
};

const comparisonFields = [
  ['firstName', 'Jméno'],
  ['lastName', 'Příjmení'],
  ['birthYear', 'Rok narození'],
  ['gender', 'Pohlaví'],
  ['email', 'E-mail'],
  ['phone', 'Telefon'],
  ['street', 'Ulice'],
  ['orientationNumber', 'Číslo orientační'],
  ['city', 'Obec'],
  ['postalCode', 'PSČ'],
  ['externalIds', 'Externí ID'],
  ['groups', 'Skupiny'],
] as const;

type ComparisonField = (typeof comparisonFields)[number][0];
export const genderLabels: Record<string, string> = { MAN: 'Muž', WOMAN: 'Žena' };

function comparisonValue(person: ComparisonPerson, field: ComparisonField) {
  if (field === 'gender') return genderLabels[person.gender ?? ''] ?? person.gender ?? '';
  const value = person[field];
  return Array.isArray(value)
    ? [...new Set(value)]
        .toSorted(field === 'groups' ? compareText : undefined)
        .join(', ')
    : String(value ?? '');
}

function nameKey(person: ComparisonPerson) {
  return `${slugify(person.firstName).trim()} ${slugify(person.lastName).trim()}`;
}

// Missing values do not conflict; retain original registrations for review.
export function groupRegistrations(registrations: ComparisonPerson[]) {
  const result: ComparisonPerson[] = [];
  for (const candidates of Map.groupBy(registrations, nameKey).values()) {
    const years = new Set(candidates.flatMap((x) => x.birthYear ?? []));
    const phones = new Set(candidates.flatMap((x) => x.phone ?? []));
    const emails = new Set(candidates.flatMap((x) => x.email ?? []));
    let groups = [candidates];
    let conflicts: string[] | undefined;
    if (years.size < 2) {
      if (phones.size >= 2 || emails.size >= 2) conflicts = ['Neshoda kontaktů'];
    } else if (years.size >= phones.size && years.size >= emails.size) {
      if (candidates.some((x) => !x.birthYear)) conflicts = ['Chybí rok narození'];
      else groups = [...Map.groupBy(candidates, (x) => x.birthYear).values()];
    } else {
      conflicts = ['Neshoda roků a kontaktů'];
    }
    if (conflicts) {
      for (const [label, values] of [
        ['Rok', candidates.map((x) => x.birthYear)],
        ['E-mail', candidates.map((x) => x.email)],
        ['Telefon', candidates.map((x) => x.phone)],
      ] as const) {
        const distinct = [...new Set(values.map((x) => x || '—'))];
        if (distinct.length > 1) conflicts.push(`${label}: ${distinct.join(' / ')}`);
      }
    }
    for (const group of groups) {
      result.push({
        ...group[0]!,
        ...(!conflicts && {
          firstName: group.find((x) => x.firstName)?.firstName || '?',
          lastName: group.find((x) => x.lastName)?.lastName || '?',
          birthYear: group.find((x) => x.birthYear)?.birthYear || null,
          gender: group.find((x) => x.gender)?.gender || 'MAN',
          email: group.find((x) => x.email)?.email || null,
          phone: group.find((x) => x.phone)?.phone || null,
          street: group.find((x) => x.street)?.street || null,
          orientationNumber:
            group.find((x) => x.orientationNumber)?.orientationNumber || null,
          city: group.find((x) => x.city)?.city || null,
          postalCode: group.find((x) => x.postalCode)?.postalCode || null,
        }),
        externalIds: group.flatMap((x) => x.externalIds),
        groups: [...new Set(group.flatMap((x) => x.groups))],
        conflicts,
        registrations: group,
      });
    }
  }
  return result.toSorted((a, b) =>
    `${a.lastName}, ${a.firstName}`.localeCompare(`${b.lastName}, ${b.firstName}`),
  );
}

export function comparePeople(source: ComparisonPerson[], database: ComparisonPerson[]) {
  const byName = Map.groupBy(database, nameKey);
  const byExternalId = new Map<string, ComparisonPerson>();
  for (const person of database) {
    for (const id of person.externalIds) {
      if (!byExternalId.has(id)) byExternalId.set(id, person);
    }
  }
  const processed = new Set<string>();
  const matches: PersonMatch[] = source.map((student) => {
    const candidates = (byName.get(nameKey(student)) ?? []).filter(
      (person) => !processed.has(person.id),
    );
    if (student.conflicts?.length) {
      const years = new Set(student.registrations?.flatMap((x) => x.birthYear ?? []));
      const candidatesByYear =
        years.size <= 1
          ? candidates.filter(
              (x) => years.size === 0 || (x.birthYear && years.has(x.birthYear)),
            )
          : [];
      const person = candidatesByYear.length === 1 ? candidatesByYear[0] : undefined;
      if (person) processed.add(person.id);
      return {
        source: student,
        database: person,
        needsReview: true,
        reason: student.conflicts[0],
        actions: [],
      };
    }
    const byYear = candidates.filter((x) => (x.birthYear ?? 1900) === student.birthYear);
    const missingYear = candidates.filter((x) => (x.birthYear ?? 1900) === 1900);

    const matchesEmail = (person: ComparisonPerson) =>
      !!student.email && person.email?.trim().toLowerCase() === student.email;
    const byEmail = candidates.filter(matchesEmail);
    const external = student.externalIds
      .map((id) => byExternalId.get(id))
      .find((person) => person && !processed.has(person.id));
    // An old Starlet identity can survive on a retired duplicate after a rename.
    const current =
      external?.isFormer && student.birthYear
        ? candidates.filter((x) => x.isMember && x.birthYear === student.birthYear)
        : [];
    const person =
      (current.length === 1 ? current[0] : undefined) ??
      external ??
      (student.birthYear ? (byYear.find(matchesEmail) ?? byYear[0]) : undefined) ??
      (byEmail.length === 1 ? byEmail[0] : undefined) ??
      missingYear.find(matchesEmail) ??
      missingYear[0] ??
      (!student.birthYear ? candidates[0] : undefined);
    if (person) processed.add(person.id);
    return {
      source: student,
      database: person,
      reason: person ? (person === external ? 'Externí ID' : 'Jméno') : undefined,
      needsReview: !person && candidates.length > 0,
      actions: [],
    };
  });
  for (const person of database) {
    if (processed.has(person.id) || person.isFormer) continue;
    const conflict = source.find(
      (student) =>
        student.conflicts?.length &&
        (nameKey(student) === nameKey(person) ||
          student.externalIds.some((id) => person.externalIds.includes(id))),
    );
    matches.push({
      database: person,
      needsReview: false,
      reason: conflict ? conflict.conflicts?.[0] : person.keepReason,
      actions: !conflict && person.unmatchedAction ? [person.unmatchedAction] : [],
    });
  }
  return matches.map(planChanges);
}

function planChanges(match: PersonMatch) {
  const { source, database } = match;
  const actions = [...match.actions];
  const differences =
    source && database && !source.conflicts?.length
      ? comparisonFields
          .filter(([field]) => field !== 'birthYear' || !database.birthDate)
          .map(([field, label]) => ({
            field,
            label,
            values: [comparisonValue(source, field), comparisonValue(database, field)],
          }))
          .filter(({ values }) => values[0] !== values[1])
      : [];
  if (source && !source.conflicts?.length) {
    if (database) {
      if (
        source.firstName !== database.firstName ||
        source.lastName !== database.lastName
      )
        actions.push('přejmenovat');
      if (
        differences.some(
          ({ field }) =>
            !['firstName', 'lastName', 'groups', 'externalIds'].includes(field),
        )
      )
        actions.push('upravit údaje');
      if (differences.some(({ field }) => field === 'groups'))
        actions.push('upravit skupiny');
      if (database.isMember === false) actions.push('obnovit členství');
      if (differences.some(({ field }) => field === 'externalIds'))
        actions.push('upravit vazbu evidence');
    } else {
      actions.push('Vytvořit');
    }
  }
  return { ...match, actions, differences };
}
