import type { Control } from 'react-hook-form';
import { z } from 'zod';

export const EventForm = z.object({
  name: z.string().prefault(''),
  type: z.enum(['CAMP', 'LESSON', 'RESERVATION', 'HOLIDAY', 'GROUP']).prefault('LESSON'),
  locationId: z.string().prefault('none'),
  locationText: z.string().prefault(''),
  capacity: z.number().prefault(0),
  capacityUnit: z.enum(['PEOPLE', 'REGISTRATIONS']).prefault('PEOPLE'),
  isVisible: z.boolean().prefault(false),
  isPublic: z.boolean().prefault(false),
  hasPublicDetails: z.boolean().prefault(false),
  enableNotes: z.boolean().prefault(false),
  isLocked: z.boolean().prefault(false),
  instances: z.array(
    z.object({
      itemId: z.string().nullable().prefault(null),
      since: z.string(),
      until: z.string(),
      isCancelled: z.boolean().prefault(false),
    }),
  ),
  trainers: z
    .array(
      z.object({
        personId: z.string(),
        lessonsOffered: z.number().nullish().prefault(null),
      }),
    )
    .prefault([]),
  cohorts: z.array(z.object({ cohortId: z.string() })).prefault([]),
  registrations: z
    .array(
      z.object({
        personId: z.string().nullable().prefault(null),
        coupleId: z.string().nullable().prefault(null),
      }),
    )
    .prefault([]),
});

export type EventFormInput = z.input<typeof EventForm>;
export type EventFormType = z.infer<typeof EventForm>;
export type EventFormControl = Control<EventFormInput, unknown, EventFormType>;
