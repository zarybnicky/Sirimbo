import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';
import { endOfMonth } from 'date-fns';
import { upsertEvent } from './federated.queries.ts';

const rangeKeyRe = /^(\d{4})-(\d{2})$/;

function parseRangeKey(key: string) {
  const match = rangeKeyRe.exec(key);
  if (!match) {
    throw new Error(`Invalid csts eventIndex key: ${key}`);
  }
  const month = new Date(Date.UTC(parseInt(match[1]), parseInt(match[2]), 1));
  return {
    from: month.toISOString().slice(0, 10),
    to: endOfMonth(month).toISOString().slice(0, 10),
  };
}

const eventSchema = z.object({
  id: z.number(),
  dateFrom: z.iso.date(),
  dateTo: z.iso.date(),
  eventCompetitions: z
    .array(
      z.object({
        id: z.number(),
        city: z.string(),
        name: z.string(),
        date: z.iso.datetime({ local: true }),
        state: z.number(),
        // TODO: Unused yet
        dayOfEvent: z.number(),
        street: z.string().optional(),
        addressNote: z.string().optional(),
        zipCode: z.string().optional(),
        post: z.string().optional(),
        gps: z.string().optional(),
        note: z.string().optional(),
        bankAccount: z.string().optional(),
        responsiblePerson: z.string(),
        organizer: z.string().optional(),
        coOrganizer: z.string().optional(),
        promoter: z.string().optional(),
        promoterWeb: z.string().optional(),
        promoterPropagation: z.string().optional(),
        supporter: z.string().optional(),
        phone: z.string().optional(),
        email: z.string().optional(),
        music: z.string().optional(),
        danceFloor: z.string().optional(),
        prizes: z.string().optional(),
        costs: z.string().optional(),
        entranceFee: z.string().optional(),
        dateApproved: z.boolean(),
        privateInCalendar: z.boolean(),
        referenceNumber: z.number().optional(),
        registrationFee: z.number().optional(),
        finalRegistrationFee: z.number().optional(),
        hallOpening: z.iso.time(),
        competitionsStart: z.iso.time(),
        juryMeeting: z.iso.datetime({ offset: true }),
        createdDate: z.iso.datetime({ offset: true }),
        updateDate: z.iso.datetime({ offset: true }),
        cancelDate: z.iso.datetime({ offset: true }).optional(),
        publishingDate: z.iso.datetime({ offset: true }).optional(),
        registrationDeadline: z.iso.datetime({ offset: true }).optional(),
        registrationTimesDeadline: z.iso.datetime({ offset: true }),
        excuseDeadline: z.iso.datetime({ offset: true }).optional(),
        resultsUploadDeadline: z.iso.datetime({ offset: true }),
        competitionCancelDeadline: z.iso.datetime({ offset: true }),
      }),
    )
    .nonempty(),
  foreignCompetitionEvents: z.array(z.object()),
  canAlsoRegister: z.string().optional(),
});

const responseSchema = z.object({
  collection: z.array(eventSchema),
});

type Response = z.output<typeof responseSchema>;

export const cstsEventIndex: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    const { from, to } = parseRangeKey(key);
    const url = new URL('https://www.csts.cz/api/1/events');
    url.searchParams.set('filter', `date>=${from} AND date<=${to}`);
    return {
      url,
      init: {
        headers: {
          referrer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  async load(client, parsed) {
    for (const event of parsed.collection) {
      const competitionEvent = event.eventCompetitions.find((x) => x)!;
      await upsertEvent.run(
        {
          federation: 'csts',
          externalId: event.id.toString(),
          country: 'Czechia',
          startDate: competitionEvent.date,
          location: competitionEvent.city,
          name: competitionEvent.name,
        },
        client,
      );
    }
    const keys = parsed.collection.map((x) => String(x.id));
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'eventCompetitions', keys },
      client,
    );
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'eventCompetitors', keys },
      client,
    );
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'eventOfficials', keys },
      client,
    );
  },
};
