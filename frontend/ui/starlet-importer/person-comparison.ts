import { compareText } from '@/lib/compare';
import { slugify } from '@/lib/slugify';
import type {
  AddressDomainInput,
  CreateTenantMembershipInput,
  GenderType,
  PersonInput,
  PersonPatch,
  UpdatePersonInput,
} from '@/graphql/index';
import type { CohortFragment } from '@/graphql/Cohorts';

export const actionLabels = {
  rename: 'Změní jméno',
  details: 'Změní údaje',
  groups: 'Změní skupiny',
  renewMembership: 'Obnoví členství',
  externalIds: 'Změní ID z evidence',
  create: 'Vytvoří',
  archive: 'Archivuje',
  endGroups: 'Ukončí členství ve skupinách z evidence',
};
export type ComparisonAction = keyof typeof actionLabels;

export type ComparisonPerson = {
  id: string;
  firstName: string;
  lastName: string;
  birthYear: number | null;
  birthDate?: string | null;
  gender?: GenderType | null;
  email: string | null;
  phone: string | null;
  externalIds: string[];
  groups: string[];
  isFormer?: boolean;
  isMember?: boolean;
  membershipId?: string;
  conflicts?: string[];
  registrations?: ComparisonPerson[];
  course?: string;
  street?: string | null;
  orientationNumber?: string | null;
  city?: string | null;
  postalCode?: string | null;
  address?: AddressDomainInput;
  cohortIds?: string[];
  keepReason?: string;
  unmatchedAction?: 'archive' | 'endGroups';
};

type PersonMatch = {
  source?: ComparisonPerson;
  database?: ComparisonPerson;
  reason?: string;
  actions: ComparisonAction[];
  needsReview: boolean;
};

type ComparisonCohort = Pick<CohortFragment, 'id' | 'name' | 'externalIds'>;
type PersonChanges = {
  create?: PersonInput;
  update?: UpdatePersonInput;
  createMembership?: CreateTenantMembershipInput;
  expireMembership?: string;
  cohortIds?: string[];
  error?: string;
};

const comparisonFields = [
  ['firstName', 'Jméno', 'rename'],
  ['lastName', 'Příjmení', 'rename'],
  ['birthYear', 'Rok narození', 'details'],
  ['gender', 'Pohlaví', 'details'],
  ['email', 'E-mail', 'details'],
  ['phone', 'Telefon', 'details'],
  ['street', 'Ulice', 'details'],
  ['orientationNumber', 'Číslo orientační', 'details'],
  ['city', 'Obec', 'details'],
  ['postalCode', 'PSČ', 'details'],
  ['externalIds', 'Externí ID', 'externalIds'],
  ['groups', 'Skupiny', 'groups'],
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

export function comparePeople(
  source: ComparisonPerson[],
  database: ComparisonPerson[],
  cohorts: ComparisonCohort[],
  tenantId: string,
) {
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
      (x) => !processed.has(x.id),
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

    const matchesEmail = (person: { email: string | null }) =>
      !!student.email && person.email?.trim().toLowerCase() === student.email;
    const byEmail = candidates.filter(matchesEmail);
    const external = student.externalIds
      .map((id) => byExternalId.get(id))
      .find((x) => x && !processed.has(x.id));

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
      (x) =>
        x.conflicts?.length &&
        (nameKey(x) === nameKey(person) ||
          x.externalIds.some((id) => person.externalIds.includes(id))),
    );
    matches.push({
      database: person,
      needsReview: false,
      reason: conflict ? conflict.conflicts?.[0] : person.keepReason,
      actions: !conflict && person.unmatchedAction ? [person.unmatchedAction] : [],
    });
  }
  return matches.map((match) => planChanges(match, cohorts, tenantId));
}

export type ComparisonRow = ReturnType<typeof comparePeople>[number];

function planChanges(match: PersonMatch, cohorts: ComparisonCohort[], tenantId: string) {
  const { source, database } = match;
  const actions = new Set(match.actions);
  const changes: PersonChanges = {};

  const differences =
    source && database && !source.conflicts?.length
      ? comparisonFields
          .filter(([field]) => field !== 'birthYear' || !database.birthDate)
          .map(([field, label, action]) => ({
            field,
            label,
            action,
            values: [comparisonValue(source, field), comparisonValue(database, field)],
          }))
          .filter(({ values }) => values[0] !== values[1])
      : [];

  if (source && !source.conflicts?.length) {
    if (database) {
      const patch: PersonPatch = {};
      for (const { field, action } of differences) {
        actions.add(action);
        switch (field) {
          case 'groups':
            break;
          case 'birthYear':
            patch.birthDate = source.birthYear ? `${source.birthYear}-01-01` : null;
            break;
          case 'gender':
            patch.gender = source.gender ?? 'MAN';
            break;
          case 'externalIds':
            patch.externalIds = [...new Set(source.externalIds)];
            break;
          case 'street':
          case 'orientationNumber':
          case 'city':
          case 'postalCode':
            patch.address ??= {
              street: database.address?.street ?? '',
              orientationNumber: database.address?.orientationNumber ?? '',
              city: database.address?.city ?? '',
              postalCode: database.address?.postalCode ?? '',
              conscriptionNumber: database.address?.conscriptionNumber ?? '',
              region: database.address?.region ?? '',
              district: database.address?.district,
            };
            patch.address[field] = source[field] ?? '';
            break;
          default:
            patch[field] = source[field];
        }
      }
      if (Object.keys(patch).length > 0) changes.update = { id: database.id, patch };
      if (database.isMember === false) {
        actions.add('renewMembership');
        changes.createMembership = {
          tenantMembership: { personId: database.id, tenantId },
        };
      }
    } else {
      actions.add('create');
      changes.create = {
        firstName: source.firstName,
        lastName: source.lastName,
        gender: source.gender ?? 'MAN',
        email: source.email,
        phone: source.phone,
        birthDate: source.birthYear ? `${source.birthYear}-01-01` : null,
        externalIds: [...new Set(source.externalIds)],
        nationality: '203',
        prefixTitle: '',
        suffixTitle: '',
        bio: '',
        address: {
          street: source.street ?? '',
          orientationNumber: source.orientationNumber ?? '',
          city: source.city ?? '',
          postalCode: source.postalCode ?? '',
          conscriptionNumber: '',
          region: '',
        },
      };
    }
  }
  if (actions.has('archive')) changes.expireMembership = database?.membershipId;
  if (actions.has('create') || actions.has('groups') || actions.has('endGroups')) {
    changes.cohortIds = (database?.cohortIds ?? []).filter(
      (id) => !cohorts.some((c) => c.id === id && c.externalIds?.length),
    );
    for (const name of source?.groups ?? []) {
      const matches = cohorts.filter((c) => c.name === name && c.externalIds?.length);
      if (matches.length === 0) changes.error ??= `Nejdříve synchronizujte kurz ${name}.`;
      changes.cohortIds.push(...matches.map((c) => c.id));
    }
  }
  return {
    ...match,
    actions: [...actions],
    differences,
    changes: !match.needsReview && Object.keys(changes).length > 0 ? changes : undefined,
  };
}
