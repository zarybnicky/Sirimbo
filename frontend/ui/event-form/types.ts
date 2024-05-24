import { z } from 'zod';

export const EventForm = z.object({
  name: z.string().default(''),
  type: z.enum(['CAMP', 'LESSON', 'RESERVATION', 'HOLIDAY', 'GROUP']),
  summary: z.string().default(''),
  description: z.string().default(''),
  descriptionMember: z.string().default(''),
  locationId: z.string().optional().nullish().default(null),
  locationText: z.string().default(''),
  capacity: z.number().nullish().default(0),
  isVisible: z.boolean().default(false),
  isPublic: z.boolean().default(false),
  enableNotes: z.boolean().default(false),
  isLocked: z.boolean().default(false),
  titleImageLegacy: z.string().nullish().default(null),
  memberPrice: z.number().nullish().default(null),
  guestPrice: z.number().nullish().default(null),
  paymentType: z.enum(['UPFRONT', 'AFTER_INSTANCE', 'NONE']),
  instances: z.array(
    z.object({
      itemId: z.string().nullish().default(null).optional(),
      date: z.string().nullish().optional().default(null),
      startTime: z.string(),
      endTime: z.string(),
      isCancelled: z.boolean().nullish().default(false),
    }),
  ),
  trainers: z.array(
    z.object({
      itemId: z.string().nullish().default(null).optional(),
      personId: z.string().nullish(),
      lessonsOffered: z.number().nullish().default(null),
    }),
  ).default([]),
  cohorts: z.array(
    z.object({
      itemId: z.string().nullish().default(null).optional(),
      cohortId: z.string().nullish(),
    }),
  ).default([]),
  registrations: z.array(
    z.object({
      itemId: z.string().nullish().default(null).optional(),
      personId: z.string().nullish().optional().default(null),
      coupleId: z.string().nullish().optional().default(null),
    }),
  ).default([]),
});
