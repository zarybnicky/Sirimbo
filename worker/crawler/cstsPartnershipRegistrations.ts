import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  competitorType,
  disciplineType,
  mapCompetitorType,
  numberAsEnum,
  seriesType,
} from './cstsEnums.ts';
import {
  type competitor_role,
  type competitor_type,
  ensurePeople,
  mergeCompetitorComponents,
  upsertCompetitorsDetailed,
} from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

const personSchema = z.object({
  idt: z.number().int().positive(),
  name: z.string(),
  surname: z.string(),
  confirmationDate: z.iso.datetime({ offset: true }).nullish(),
  classWhenLinked: z.number(),
  pointsWhenLinked: z.number(),
  finaleNationalWhenLinked: z.number(),
  finaleForeignWhenLinked: z.number(),
});

const schema = z.object({
  collection: z.array(
    z.object({
      id: z.number().int().positive(),
      competitorId: z.number().int().positive(),
      competitorType: numberAsEnum(competitorType).pipe(
        z.enum(['Couple', 'ProAm', 'Duo', 'DuoF']),
      ),
      competitorAge: z.number(),
      series: numberAsEnum(seriesType),
      discipline: numberAsEnum(disciplineType),
      rankingAge: z.number(),
      state: z.number().int(),
      selectedClass: z.number(),
      registrationDate: z.iso.datetime({ offset: true }).nullish(),
      separationDate: z.iso.datetime({ offset: true }).nullish(),
      createdDate: z.iso.datetime({ offset: true }),
      person1: personSchema,
      person2: personSchema,
    }),
  ),
  paging: z.object({ page: z.number(), pageSize: z.number(), totalCount: z.number() }),
});

export const cstsPartnershipRegistrations: JsonLoader<z.output<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest(key) {
    const url = new URL(
      'https://www.csts.cz/api/1/competitors/partnership_registrations',
    );
    url.searchParams.set('filter', `idt=${key}`);
    return {
      url,
      init: { headers: { Referer: 'https://www.csts.cz/dancesport/evidence/lide' } },
    };
  },
  async load(client, parsed) {
    if (parsed.paging.totalCount > parsed.collection.length) {
      throw new Error('ČSTS partnership registrations response is incomplete');
    }
    const competitors = makePgtypedCollection<{
      federation: string;
      externalId: string;
      label: string;
      type: competitor_type;
      ageGroup: string;
      divisions: string;
      active: string;
      since: string;
      until: string;
    }>(
      [
        'federation',
        'externalId',
        'label',
        'type',
        'ageGroup',
        'divisions',
        'active',
        'since',
        'until',
      ],
      ['federation', 'externalId'],
    );
    const people = makePgtypedCollection<{
      federation: string;
      externalId: string;
      canonicalName: string;
      gender: 'unknown';
    }>(
      ['federation', 'externalId', 'canonicalName', 'gender'],
      ['federation', 'externalId'],
    );
    const components = makePgtypedCollection<{
      competitorId: string;
      personId: string;
      role: competitor_role;
    }>(['competitorId', 'personId', 'role'], ['competitorId', 'personId']);
    for (const registrations of Map.groupBy(
      parsed.collection,
      (r) => r.competitorId,
    ).values()) {
      const registration = registrations[0]!;
      const id = `csts:${registration.competitorId}`;
      const active = registrations.some((r) => r.state === 1)
        ? true
        : registrations.every((r) => r.state === 2)
          ? false
          : null;
      const type = mapCompetitorType(registration.competitorType);
      const members = [registration.person1, registration.person2];
      competitors.add({
        federation: 'csts',
        externalId: registration.competitorId.toString(),
        label: members
          .map((person) => `${person.name} ${person.surname}`.trim())
          .join(' - '),
        type,
        ageGroup: '',
        divisions: '[]',
        active: active === null ? '' : String(active),
        since:
          registrations
            .flatMap((r) => (r.registrationDate ? [r.registrationDate] : []))
            .toSorted((a, b) => Date.parse(a) - Date.parse(b))[0] ?? '',
        until:
          active === false && registrations.every((r) => r.separationDate)
            ? registrations
                .map((r) => r.separationDate!)
                .toSorted((a, b) => Date.parse(b) - Date.parse(a))[0]!
            : '',
      });
      for (const [index, person] of members.entries()) {
        people.add({
          federation: 'csts',
          externalId: person.idt.toString(),
          canonicalName: `${person.name} ${person.surname}`.trim(),
          gender: 'unknown',
        });
        components.add({
          competitorId: id,
          personId: `csts:${person.idt}`,
          role: type === 'couple' ? (index === 0 ? 'lead' : 'follow') : 'member',
        });
      }
    }
    if (people.length) await ensurePeople.run(people.params, client);
    if (competitors.length)
      await upsertCompetitorsDetailed.run(competitors.params, client);
    if (components.length) await mergeCompetitorComponents.run(components.params, client);
    return {
      upsertFrontier: people.params.externalId.map((key) => ({
        federation: 'csts',
        kind: 'member',
        key,
      })),
    };
  },
};
