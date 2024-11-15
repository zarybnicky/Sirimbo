import type { CohortBasicFragment } from '@/graphql/Cohorts';
import React from 'react';
import { truthyFilter } from './truthyFilter';

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
