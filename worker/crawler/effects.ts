import type { PoolClient } from 'pg';
import { upsertFrontiers } from './crawler.queries.ts';
import { refreshCompetitionRoundResults } from './federated.queries.ts';
import {
  makePgtypedCollection,
  type PgtypedCollection,
} from './pgtypedCollection.ts';

type RoundResultRefreshEffect = {
  competitionId: string;
};

type FrontierUpsertEffect = {
  federation: string;
  kind: string;
  key: string;
};

type FrontierUpsertParams = {
  federations: string[];
  kinds: string[];
  keys: string[];
};

type FrontierUpsertBuffer = {
  add(...items: FrontierUpsertEffect[]): void;
  readonly params: FrontierUpsertParams;
  readonly length: number;
};

export type LoaderEffects = {
  roundResultRefreshes: PgtypedCollection<RoundResultRefreshEffect>;
  frontierUpserts: FrontierUpsertBuffer;
};

export type LoaderResult = LoaderEffects | void;

export function createLoaderEffects(): LoaderEffects {
  return {
    roundResultRefreshes: makePgtypedCollection(
      ['competitionId'],
      ['competitionId'],
    ),
    frontierUpserts: createFrontierUpsertBuffer(),
  };
}

function createFrontierUpsertBuffer(): FrontierUpsertBuffer {
  const collection = makePgtypedCollection<FrontierUpsertEffect>(
    ['federation', 'kind', 'key'],
    ['federation', 'kind', 'key'],
  );

  return {
    add(...items) {
      collection.add(...items);
    },
    get params() {
      return {
        federations: collection.params.federation,
        kinds: collection.params.kind,
        keys: collection.params.key,
      };
    },
    get length() {
      return collection.length;
    },
  };
}

export function mergeLoaderEffects(
  target: LoaderEffects,
  source: LoaderResult,
) {
  if (!source) return;

  for (const competitionId of source.roundResultRefreshes.params.competitionId) {
    target.roundResultRefreshes.add({ competitionId });
  }

  const { federations, kinds, keys } = source.frontierUpserts.params;
  for (let i = 0; i < keys.length; i++) {
    target.frontierUpserts.add({
      federation: federations[i],
      kind: kinds[i],
      key: keys[i],
    });
  }
}

export async function flushLoaderEffects(
  client: PoolClient,
  effects: LoaderEffects,
) {
  if (effects.frontierUpserts.length) {
    await upsertFrontiers.run(effects.frontierUpserts.params, client);
  }

  for (const competitionId of effects.roundResultRefreshes.params.competitionId) {
    await refreshCompetitionRoundResults.run({ competitionId }, client);
  }
}
