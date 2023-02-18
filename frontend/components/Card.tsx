import classNames from "classnames";
import { CohortFragment } from "lib/graphql/Cohorts";
import React from "react";

export function Card({ cohort, children, ...props }: {
  cohort?: CohortFragment;
} & React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      {...props}
      className={classNames(
        "bg-white relative border border-stone-200 shadow-sm sm:rounded-lg p-3 mb-2",
        cohort ? 'pl-8' : '',
        props.className,
      )}>
      {children}
      {cohort && (
        <div className="absolute rounded-l-lg w-4 border-r border-stone-200 shadow-sm top-0 bottom-0 left-0" style={{ backgroundColor: cohort.sColorRgb }} />
      )}
    </div>
  );
};
