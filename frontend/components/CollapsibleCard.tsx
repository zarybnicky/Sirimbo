import * as Collapsible from '@radix-ui/react-collapsible';
import classNames from 'classnames';
import { CohortFragment } from 'lib/graphql/Cohorts';
import React from 'react';
import { ChevronDown } from 'react-feather';

interface CollapsibleCardProps
  extends Omit<React.HTMLAttributes<HTMLDivElement>, 'title'> {
  title: React.ReactNode;
  cohort?: CohortFragment;
}

export function CollapsibleCard({
  title,
  cohort,
  children,
  ...props
}: CollapsibleCardProps) {
  return (
      <Collapsible.Root className={classNames("relative border flex flex-col mb-1 rounded-lg", cohort && 'pl-3')}>
      <Collapsible.Trigger className="group flex justify-between p-2">
      <div>{title}</div>
        <ChevronDown className="text-stone-400 transform duration-300 ease-in-out group-radix-state-open:rotate-180" />
      </Collapsible.Trigger>
      <Collapsible.Content className="CollapsibleContent">
        <div className="p-2">{children}</div>
      </Collapsible.Content>
      {cohort && (
        <div
          className="absolute rounded-l-lg w-3 border-r border-stone-200 shadow-sm top-0 bottom-0 left-0"
          style={{ backgroundColor: cohort.sColorRgb }}
        />
      )}
    </Collapsible.Root>
  );
}
