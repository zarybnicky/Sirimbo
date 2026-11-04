import type { PoolClient } from 'pg';
import { upsertFrontiers } from './crawler.queries.ts';
import { refreshCompetitionRoundResults, refreshWdsfRoundDances } from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

type FrontierUpsert = {
  federation: string;
  kind: string;
  key: string;
  referenceDate?: string;
};

export type LoaderEffect = {
  referenceDate?: string;
  upsertFrontier?: readonly FrontierUpsert[];
  refreshRoundResult?: readonly string[];
};

export type LoaderResult = LoaderEffect | void;

export async function flushLoaderEffects(
  client: PoolClient,
  effects: readonly LoaderEffect[],
) {
  const frontiers = new Map<string, FrontierUpsert>();
  const frontierUpserts = makePgtypedCollection<Required<FrontierUpsert>>(
    ['federation', 'kind', 'key', 'referenceDate'],
  );
  const roundResultRefreshes = new Set<string>();

  for (const effect of effects) {
    for (const frontier of effect.upsertFrontier ?? []) {
      const key = [frontier.federation, frontier.kind, frontier.key].join('\0');
      frontiers.set(key, {
        ...frontier,
        referenceDate: frontier.referenceDate ?? frontiers.get(key)?.referenceDate,
      });
    }
    for (const competitionId of effect.refreshRoundResult ?? []) {
      roundResultRefreshes.add(competitionId);
    }
  }

  for (const frontier of frontiers.values()) {
    frontierUpserts.add({ ...frontier, referenceDate: frontier.referenceDate ?? '' });
  }

  if (frontierUpserts.length) {
    await upsertFrontiers.run(
      {
        federations: frontierUpserts.params.federation,
        kinds: frontierUpserts.params.kind,
        keys: frontierUpserts.params.key,
        referenceDates: frontierUpserts.params.referenceDate,
      },
      client,
    );
  }

  for (const competitionId of roundResultRefreshes) {
    await refreshWdsfRoundDances.run({ competitionId }, client);
    await refreshCompetitionRoundResults.run({ competitionId }, client);
  }
}
