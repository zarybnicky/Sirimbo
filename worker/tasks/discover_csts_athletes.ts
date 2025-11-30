import type { Task } from 'graphile-worker';
import { getNextIdt } from '../crawler/cstsAthleteIdts.ts';
import { cstsAthlete } from '../crawler/cstsAthlete.ts';
import type { FrontierRow } from '../crawler/types.ts';
import { insertDiscoveredCstsMember } from '../crawler/crawler.queries.ts';

const MAX_PROBES_PER_RUN = 200;
const HOLE_LIMIT = 200;

export const discover_csts_athletes: Task<'discover_csts_athletes'> = async (
  _payload,
  helpers,
) => {
  const { rows } = await helpers.query(
    `SELECT last_known FROM crawler.incremental_ranges WHERE federation = 'csts' AND kind = 'member_id'`,
  );
  if (rows.length === 0) {
    helpers.logger.warn(`[IDT probe] No incremental range found for csts/member_id`);
    return;
  }

  let probes = 0;
  let holeCount = 0;
  let newLastChecked = rows[0].last_known;

  while (probes < MAX_PROBES_PER_RUN) {
    const candidateId = getNextIdt(newLastChecked);
    if (candidateId == null) {
      helpers.logger.warn(`[IDT probe] No more IDTs to probe`);
      break;
    }

    probes += 1;
    newLastChecked = candidateId;

    const request = cstsAthlete.buildRequest({
      key: candidateId.toString(),
    } as FrontierRow);
    const response = await fetch(request.url, request.init);

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
      helpers.logger.info(
        `[IDT probe] Found ${candidateId} (${probes}/${MAX_PROBES_PER_RUN})`,
      );
      await helpers.withPgClient(async (client) => {
        await insertDiscoveredCstsMember.run({ id: candidateId.toString() }, client);
      });

      holeCount = 0; // reset hole streak
    } else {
      helpers.logger.info(
        `[IDT probe] Missed ${candidateId} (${probes}/${MAX_PROBES_PER_RUN})`,
      );
      holeCount += 1;
      if (holeCount >= HOLE_LIMIT) {
        // assume we've hit a long gap; stop early
        helpers.logger.warn(`[IDT probe] Hit hole limit at IDT ${candidateId}`);
        break;
      }
    }
  }

  await helpers.query(
    `UPDATE crawler.incremental_ranges SET last_checked = $2 WHERE federation = 'csts' AND kind = 'member_id'`,
    [newLastChecked],
  );
};

export default discover_csts_athletes;
