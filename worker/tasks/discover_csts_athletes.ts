import type { Task } from 'graphile-worker';
import { getNextIdt } from '../crawler/cstsAthleteIdts.ts';
import { upsertFrontier } from '../crawler/crawler.queries.ts';
import { LOADER_MAP } from '../crawler/handlers.ts';

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

    probes += 1;
    newLastChecked = candidateId;

    const { url, init } = LOADER_MAP.csts.member.buildRequest(candidateId.toString());
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
        await upsertFrontier.run(
          { federation: 'csts', kind: 'member', key: candidateId.toString() },
          client,
        );
        await client.query(
          `UPDATE crawler.incremental_ranges SET last_known = $1 WHERE federation = 'csts' AND kind = 'member_id'`,
          [candidateId],
        );
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
