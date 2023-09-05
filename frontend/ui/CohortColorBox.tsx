import { CohortBasicFragment } from '@app/graphql/Cohorts';
import React from 'react';

const truthyFilter = Boolean as any as <T>(x: T | false | undefined | null | "" | 0) => x is T;

export function CohortColorBoxes({ items }: {
  items?: (CohortBasicFragment | null)[] | null;
}) {
  return (
    <div className="flex gap-0.5">
      {items?.filter(truthyFilter).map((g) =>
        <div
          className="w-3 h-3 border border-neutral-6"
          key={g.id}
          title={g.sName}
          style={{ backgroundColor: g.sColorRgb }}
        />
      )}
    </div>
  );
}
