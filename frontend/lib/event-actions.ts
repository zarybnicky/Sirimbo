'use server';

import { campCloneEvents, cloneCampEvent, copyEventMetadata, populateEventCohorts } from './event-actions.queries';
import { withRequestPgClient } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { redirect } from 'next/navigation';

export async function cloneCampAction(
  sourceId: string,
  values: { since: string; copyCohorts: boolean; copyTrainers: boolean },
) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return { error: 'Přístup odepřen.' };

  let campId: string;
  try {
    campId = await withRequestPgClient(async (client) => {
      const events = await campCloneEvents.run({ sourceId, since: values.since }, client);
      if (events.length === 0) throw new Error('Soustředění nebylo nalezeno.');

      const ids = new Map<string, string>();
      for (const event of events) {
        const [saved] = await cloneCampEvent.run({
          sourceId: event.id,
          parentId: event.parent_id ? ids.get(event.parent_id) ?? null : null,
          shift: event.shift,
          copyTrainers: values.copyTrainers,
          copyCohorts: values.copyCohorts,
        }, client);
        const copyId = saved!.id;
        ids.set(event.id, copyId);
        await copyEventMetadata.run({ copyId, sourceId: event.id }, client);
      }

      if (values.copyCohorts) {
        await populateEventCohorts.run({ ids: [...ids.values()] }, client);
      }
      return ids.get(events[0]!.id)!;
    });
  } catch (error) {
    return { error: error instanceof Error ? error.message : 'Kopírování se nezdařilo.' };
  }
  redirect(`/termin/${campId}`);
}
