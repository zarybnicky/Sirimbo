import { CohortBasicFragment } from '@/graphql/Cohorts';
import React from 'react';

const truthyFilter = Boolean as any as <T>(x: T | false | undefined | null | "" | 0) => x is T;

export function CohortColorBoxes({ items }: {
  items?: (CohortBasicFragment | null)[] | null;
}) {
  return (
    <div className="flex gap-0.5">
      {items?.filter(truthyFilter).map((g) =>
        <div
          className="size-3 border border-neutral-6"
          key={g.id}
          title={g.name}
          style={{ backgroundColor: g.colorRgb }}
        />
      )}
    </div>
  );
}
