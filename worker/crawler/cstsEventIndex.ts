import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import { upsertFrontierKeys } from './crawler.queries.ts';
import { endOfMonth } from 'date-fns';
import { upsertEventsDetailed } from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

const rangeKeyRe = /^(\d{4})-(\d{2})$/;
const text = z.string().transform((value) => value.trim());
const optionalText = z.string().nullish().transform((value) => {
  const text = value?.trim();
  return text ?? '';
});

function parseRangeKey(key: string) {
  const match = rangeKeyRe.exec(key);
  if (!match) {
    throw new Error(`Invalid csts eventIndex key: ${key}`);
  }
  const month = new Date(Date.UTC(parseInt(match[1]), parseInt(match[2]) - 1, 1));
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
        city: text,
        name: text,
        date: z.iso.datetime({ local: true }),
        state: z.number(),
        // TODO: Unused yet
        dayOfEvent: z.number(),
        street: optionalText,
        addressNote: optionalText,
        zipCode: optionalText,
        post: z.string().optional(),
        gps: optionalText,
        note: z.string().optional(),
        bankAccount: z.string().optional(),
        responsiblePerson: optionalText,
        organizer: z.string().optional(),
        coOrganizer: z.string().optional(),
        promoter: z.string().optional(),
        promoterWeb: optionalText,
        promoterPropagation: optionalText,
        supporter: z.string().optional(),
        phone: optionalText,
        email: optionalText,
        music: z.string().optional(),
        danceFloor: optionalText,
        prizes: z.string().optional(),
        costs: z.string().optional(),
        entranceFee: z.string().optional(),
        dateApproved: z.boolean(),
        privateInCalendar: z.boolean(),
        referenceNumber: z.number().optional(),
        registrationFee: z.number().optional(),
        finalRegistrationFee: z.number().optional(),
        hallOpening: z.iso.time().optional(),
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
          Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  async load(client, parsed) {
    const events = makePgtypedCollection<{
      federation: string;
      externalId: string;
      name: string;
      startDate: string;
      endDate: string;
      location: string;
      city: string;
      country: string;
      streetAddress: string;
      postalCode: string;
      addressNote: string;
      geoReference: string;
      floorSize: string;
      contactName: string;
      contactPhone: string;
      contactEmail: string;
      websiteUrl: string;
      organizingClubId: string;
    }>(
      [
        'federation',
        'externalId',
        'name',
        'startDate',
        'endDate',
        'location',
        'city',
        'country',
        'streetAddress',
        'postalCode',
        'addressNote',
        'geoReference',
        'floorSize',
        'contactName',
        'contactPhone',
        'contactEmail',
        'websiteUrl',
        'organizingClubId',
      ],
      ['federation', 'externalId'],
    );

    for (const event of parsed.collection) {
      const competitionEvent = event.eventCompetitions.find((x) => x)!;
      events.add({
        federation: 'csts',
        externalId: event.id.toString(),
        name: competitionEvent.name,
        startDate: competitionEvent.date,
        endDate: event.dateTo,
        location: competitionEvent.city,
        city: competitionEvent.city,
        country: 'Czechia',
        streetAddress: competitionEvent.street,
        postalCode: competitionEvent.zipCode,
        addressNote: competitionEvent.addressNote,
        geoReference: competitionEvent.gps,
        floorSize: competitionEvent.danceFloor,
        contactName: competitionEvent.responsiblePerson,
        contactPhone: competitionEvent.phone,
        contactEmail: competitionEvent.email,
        websiteUrl: competitionEvent.promoterWeb || competitionEvent.promoterPropagation,
        organizingClubId: '',
      });
    }
    if (events.length) await upsertEventsDetailed.run(events.params, client);

    const keys = parsed.collection.map((x) => String(x.id));
    await upsertFrontierKeys.run(
      { federation: 'csts', kind: 'event', keys },
      client,
    );
  },
};
