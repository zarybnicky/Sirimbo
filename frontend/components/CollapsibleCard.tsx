import * as Collapsible from '@radix-ui/react-collapsible';
import classNames from 'classnames';
import { CohortFragment } from '@app/graphql/Cohorts';
import React from 'react';
import { ChevronDown } from 'lucide-react';

interface CollapsibleCardProps {
  title: React.ReactNode;
  cohort?: CohortFragment;
  children: React.ReactNode;
}

export function CollapsibleCard({title, cohort, children}: CollapsibleCardProps) {
  return (
    <Collapsible.Root
      className={classNames(
        'relative border flex flex-col mb-1 rounded-lg bg-white',
        cohort && 'pl-3',
      )}
    >
      <Collapsible.Trigger className="group flex justify-between p-2">
        <div>{title}</div>
        <ChevronDown className="text-stone-400 duration-300 ease-in-out group:data-[state=open]:rotate-180" />
      </Collapsible.Trigger>
      <Collapsible.Content className="CollapsibleContent">
        <div className="p-2">{children}</div>
      </Collapsible.Content>
      {cohort && (
        <div
          className="absolute rounded-l-lg w-3 border-r border-stone-200 shadow-sm inset-y-0 left-0"
          style={{ backgroundColor: cohort.sColorRgb }}
        />
      )}
    </Collapsible.Root>
  );
}