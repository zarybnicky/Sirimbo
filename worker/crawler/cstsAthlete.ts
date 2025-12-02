import { z } from 'zod';
import { defaultMapResponseToStatus, type JsonLoader } from './types.ts';
import {
  type gender,
  upsertCategory,
  upsertFederationAthlete,
  upsertFederationCouple,
  upsertFederationCoupleProgress,
} from './federated.queries.ts';

const rankingPointsSchema = z.object({
  id: z.number().optional(),
  idt: z.number().optional(),
  age: z.string().optional(),
  class: z.string().optional(),
  points: z.number().optional(),
  partner: z.string().optional(),
  partnerIdt: z.number().optional(),
  series: z.string(),
  discipline: z.string(),
  competitors: z.string(),
  competitorId: z.number(),
  time: z.iso.datetime({ offset: true }).optional(),

  personalApproved: z.boolean().optional(),
  personalClass: z.string().optional().prefault('-'),
  personalPoints: z.number().optional().prefault(0),
  personalDomesticFinaleCount: z.number().optional().prefault(0),
  personalForeignFinaleCount: z.number().optional().prefault(0),
  medicalCheckupExpiration: z.iso.date().nullable().optional().prefault(null),

  domesticFinaleCount: z.number().optional(),
  foreignFinaleCount: z.number().optional(),

  rankingAge: z.string(),
  rankingPointsAge: z.string(),

  ranklistPoints: z.number().optional(),
  ranklistRanking: z.number().optional(),
});

const athleteSchema = z.object({
  idt: z.number(),
  name: z.string(),
  age: z.string(),
  sex: z
    .enum(['M', 'F', 'male', 'female'])
    .transform((x): gender => ({
      M: 'male',
      F: 'female',
      male: 'male',
      female: 'female'
    }[x] ?? 'unknown') as gender)
    .optional(),
  medicalCheckupExpiration: z.iso.date().nullable().optional().prefault(null),
  rankingPoints: z.array(rankingPointsSchema).optional().prefault([]),
  lat: rankingPointsSchema.optional(),
  stt: rankingPointsSchema.optional(),

  validFor: z.iso.date().optional(),
  barcode: z.string().optional(),
  avatar: z.string().optional(),
});

const athletesResponseSchema = z.object({
  collection: z.array(athleteSchema),
});

type Response = z.infer<typeof athletesResponseSchema>;

export const cstsAthlete: JsonLoader<Response> = {
  mode: 'json',
  schema: athletesResponseSchema,
  buildRequest: ({ key }) => ({
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
    delete (parsed.collection[0] as any)['validFor'];
    return parsed;
  },
  revalidatePeriod: '1 day',
  async load(client, frontier, parsed) {
    const data = parsed.collection[0];
    const [{ athlete_id: mainAthleteId }] = await upsertFederationAthlete.run(
      {
        federation: 'csts',
        externalId: String(data.idt),
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
      [String(data.idt), data.age, data.medicalCheckupExpiration],
    );

    for (const rp of data.rankingPoints) {
      if (rp.competitors !== 'Couple' || !rp.partnerIdt) continue;

      const [{ athlete_id: partnerAthleteId }] = await upsertFederationAthlete.run(
        {
          federation: 'csts',
          externalId: String(rp.partnerIdt),
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
          federatedLeadId: data.sex === 'male' ? mainAthleteId : partnerAthleteId,
          federatedFollowerId: data.sex === 'male' ? partnerAthleteId : mainAthleteId,
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
          categoryId,
          points: rp.points,
          domesticFinale: rp.domesticFinaleCount,
          foreignFinale: rp.foreignFinaleCount,
        },
        client,
      );
    }
  },
};
