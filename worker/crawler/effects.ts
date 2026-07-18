import type { PoolClient } from 'pg';
import { upsertFrontiers } from './crawler.queries.ts';
import { refreshCompetitionRoundResults } from './federated.queries.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';

type FrontierUpsert = {
  federation: string;
  kind: string;
  key: string;
};

export type LoaderEffect = {
  upsertFrontier?: readonly FrontierUpsert[];
  refreshRoundResult?: readonly string[];
};

export type LoaderResult = LoaderEffect | void;

export async function flushLoaderEffects(
  client: PoolClient,
  effects: readonly LoaderEffect[],
) {
  const frontierUpserts = makePgtypedCollection<FrontierUpsert>(
    ['federation', 'kind', 'key'],
    ['federation', 'kind', 'key'],
  );
  const roundResultRefreshes = new Set<string>();

  for (const effect of effects) {
    if (effect.upsertFrontier) frontierUpserts.add(...effect.upsertFrontier);
    for (const competitionId of effect.refreshRoundResult ?? []) {
      roundResultRefreshes.add(competitionId);
    }
  }

  if (frontierUpserts.length) {
    await upsertFrontiers.run(
      {
        federations: frontierUpserts.params.federation,
        kinds: frontierUpserts.params.kind,
        keys: frontierUpserts.params.key,
      },
      client,
    );
  }

  for (const competitionId of roundResultRefreshes) {
    await refreshCompetitionRoundResults.run({ competitionId }, client);
  }
}
