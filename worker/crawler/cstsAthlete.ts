import { z } from 'zod';
import { defaultMapResponseToStatus, type JsonLoader } from './types.ts';
import {
  upsertCategory,
  upsertFederationAthlete,
  upsertFederationCouple,
  upsertFederationCoupleProgress,
} from './crawler.queries.ts';

const rankingPointsSchema = z.object({
  rankingPointsAge: z.string(),
  personalClass: z.string().optional().prefault('-'),
  personalPoints: z.number().optional().prefault(0),
  personalDomesticFinaleCount: z.number().optional().prefault(0),
  personalForeignFinaleCount: z.number().optional().prefault(0),
  competitorId: z.number(),
  series: z.string(),
  discipline: z.string(),
  rankingAge: z.string(),
  competitors: z.string(),

  domesticFinaleCount: z.number().optional(),
  foreignFinaleCount: z.number().optional(),

  ranklistRanking: z.number().optional(),
  ranklistPoints: z.number().optional(),

  id: z.number().optional(),
  idt: z.number().optional(),
  age: z.string().optional(),
  class: z.string().optional(),
  points: z.number().optional(),
  partner: z.string().optional(),
  partnerIdt: z.number().optional(),
});

const athleteSchema = z.object({
  idt: z.number(),
  name: z.string(),
  age: z.string(),
  sex: z
    .enum(['M', 'F'])
    .transform((x) => (x === 'M' ? 'male' : x === 'F' ? 'female' : 'unknown'))
    .optional(),
  medicalCheckupExpiration: z.string().nullable().optional().prefault(null),
  rankingPoints: z.array(rankingPointsSchema),
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema),
});

type Response = z.infer<typeof athletesResponseSchema>;
type Athlete = Response['collection'][0];

export const cstsAthlete: JsonLoader<Athlete, Response> = {
  mode: 'json',
  schema: athletesResponseSchema,
  buildRequest: ({ key }: { key: string }) => ({
    url: `https://www.csts.cz/api/1/athletes/${key}`,
    init: {
      referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
    },
  }),
  mapResponseToStatus(args) {
    if (!args.parsed?.collection.length) return 'gone';
    return defaultMapResponseToStatus(args);
  },
  transformResponse(url, parsed) {
    return athleteSchema.strip().parse(parsed.collection[0]);
  },
  revalidatePeriod: '1 day',
  async load(client, url, data) {
    await upsertFederationAthlete.run(
      {
        federation: 'csts',
        externalId: data.idt.toString(),
        canonicalName: data.name,
        gender: data.sex,
      },
      client,
    );
    await client.query(
      `
        UPDATE federated.federation_athlete fa
        SET age_group = $2, medical_checkup_expiration = $3
        WHERE federation = 'csts' AND external_id = $1
      `,
      [data.idt.toString(), data.age, data.medicalCheckupExpiration],
    );

    for (const rp of data.rankingPoints) {
      if (rp.competitors !== 'Couple' || !rp.partnerIdt) continue;

      const partnerIdt = rp.partnerIdt.toString();
      await upsertFederationAthlete.run(
        {
          federation: 'csts',
          externalId: partnerIdt,
          canonicalName: rp.partner,
          gender: data.sex === 'male' ? 'female' : 'male',
        },
        client,
      );

      const [{ id: categoryId }] = await upsertCategory.run(
        {
          class: rp.class,
          ageGroup: rp.rankingAge,
          genderGroup: 'mixed',
          discipline: rp.discipline,
          series: rp.series,
        },
        client,
      );

      const [{ competitor_id: federatedCompetitorId }] = await upsertFederationCouple.run(
        {
          federation: 'csts',
          externalCompetitorId: rp.competitorId.toString(),
          externalLeadId: data.sex === 'male' ? data.idt.toString() : partnerIdt,
          externalFollowerId: data.sex === 'male' ? partnerIdt : data.idt.toString(),
          competitorLabel:
            data.sex === 'male'
              ? `${data.name} - ${rp.partner}`
              : `${rp.partner} - ${data.name}`,
        },
        client,
      );
      await upsertFederationCoupleProgress.run(
        {
          federation: 'csts',
          federatedCompetitorId,
          categoryId: categoryId.toString(),
          points: rp.points,
          domesticFinale: rp.domesticFinaleCount,
          foreignFinale: rp.foreignFinaleCount,
        },
        client,
      );
    }
  },
};
