import type { Task } from 'graphile-worker';
import { getNextIdt } from '../crawler/cstsAthleteIdts.ts';
import { cstsAthlete } from '../crawler/cstsAthlete.ts';
import type { FrontierRow } from '../crawler/types.ts';
import { insertDiscoveredCstsMember } from '../crawler/crawler.queries.ts';
import { getReservation } from '../crawler/getReservation.ts';

const MAX_PROBES_PER_RUN = 200;
const HOLE_LIMIT = 200;

export const discover_csts_athletes: Task<'discover_csts_athletes'> = async (
  _payload,
  helpers,
) => {
  const { logger } = helpers;
  const { rows } = await helpers.query(
    `SELECT last_known FROM crawler.incremental_ranges WHERE federation = 'csts' AND kind = 'member_id'`,
  );
  if (rows.length === 0) {
    logger.warn(`[IDT probe] No incremental range found for csts/member_id`);
    return;
  }

  let probes = 0;
  let holeCount = 0;
  let newLastChecked = rows[0].last_known;

  while (probes < MAX_PROBES_PER_RUN) {
    const candidateId = getNextIdt(newLastChecked);
    if (!candidateId) {
      logger.warn(`[IDT probe] No more IDTs to probe, last IDT: ${newLastChecked}`);
      break;
    }

    probes += 1;
    newLastChecked = candidateId;

    const { url, init } = cstsAthlete.buildRequest({
      key: candidateId.toString(),
    } as FrontierRow);

    while (true) {
      const reservation = await helpers.withPgClient(async (client) => {
        return getReservation(url, client);
      });
      if (reservation.proceed) break;
      await new Promise((resolve) =>
        setTimeout(resolve, reservation.runAt.getTime() - Date.now()),
      );
    }

    const response = await fetch(url, init);

    let exists = true;
    if (!response.ok) exists = false;
    const body = await response.json();
    if (
      !body ||
      typeof body !== 'object' ||
      !('collection' in body) ||
      !Array.isArray(body['collection']) ||
      body['collection'].length !== 1
    )
      exists = false;

    if (exists) {
      logger.info(`[IDT probe] Found ${candidateId} (${probes}/${MAX_PROBES_PER_RUN})`);
      await helpers.withPgClient(async (client) => {
        await insertDiscoveredCstsMember.run({ id: candidateId.toString() }, client);
      });
      holeCount = 0; // reset hole streak
    } else {
      logger.info(`[IDT probe] Missed ${candidateId} (${probes}/${MAX_PROBES_PER_RUN})`);
      holeCount += 1;
      if (holeCount >= HOLE_LIMIT) {
        logger.warn(`[IDT probe] Hit hole limit at IDT ${candidateId}`);
        break;
      }
    }
  }

  await helpers.query(
    `UPDATE crawler.incremental_ranges SET last_checked = $1 WHERE federation = 'csts' AND kind = 'member_id'`,
    [newLastChecked],
  );
};

export default discover_csts_athletes;
