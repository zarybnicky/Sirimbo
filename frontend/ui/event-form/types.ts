import { z } from 'zod';

export type EventFormType = z.infer<typeof EventForm>;

export const EventForm = z.object({
  name: z.string().prefault(''),
  type: z.enum(['CAMP', 'LESSON', 'RESERVATION', 'HOLIDAY', 'GROUP']),
  summary: z.string().prefault(''),
  description: z.string().prefault(''),
  descriptionMember: z.string().prefault(''),
  locationId: z.string().nullish().prefault(null),
  locationText: z.string().prefault(''),
  capacity: z.number().nullish().prefault(0),
  isVisible: z.boolean().prefault(false),
  isPublic: z.boolean().prefault(false),
  enableNotes: z.boolean().prefault(false),
  isLocked: z.boolean().prefault(false),
  titleImageLegacy: z.string().nullish().prefault(null),
  instances: z.array(
    z.object({
      itemId: z.string().nullish().prefault(null),
      date: z.string().nullish().prefault(null),
      endDate: z.string().nullish().prefault(null),
      startTime: z.string(),
      endTime: z.string(),
      isCancelled: z.boolean().nullish().prefault(false),
      trainers: z
        .array(
          z.object({
            itemId: z.string().nullish().prefault(null),
            personId: z.string().nullish(),
          }),
        )
        .prefault([]),
    }),
  ),
  trainers: z
    .array(
      z.object({
        itemId: z.string().nullish().prefault(null),
        personId: z.string().nullish(),
        lessonsOffered: z.number().nullish().prefault(null),
      }),
    )
    .prefault([]),
  cohorts: z
    .array(
      z.object({
        itemId: z.string().nullish().prefault(null).optional(),
        cohortId: z.string().nullish(),
      }),
    )
    .prefault([]),
  registrations: z
    .array(
      z.object({
        itemId: z.string().nullish().prefault(null).optional(),
        personId: z.string().nullish().optional().prefault(null),
        coupleId: z.string().nullish().optional().prefault(null),
      }),
    )
    .prefault([]),
});
