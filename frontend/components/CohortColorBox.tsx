import { CohortBasicFragment } from "lib/graphql/Cohorts";
import React from "react";

export function CohortColorBoxes({ items }: {
  items?: (CohortBasicFragment | null)[] | null;
}) {
  if (!items || !items.length) {
    return null;
  }
  return (
    <div className="flex gap-1 border border-gray-300">
      {items.map((g, i) => !g ? <React.Fragment key={i} /> : (
        <div className="w-3 h-3"
          key={g.sColorRgb}
          title={g.sName}
          style={{ backgroundColor: g.sColorRgb }}
        />
      ))}
    </div>
  );
}
