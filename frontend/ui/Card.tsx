import { CohortFragment } from '@/graphql/Cohorts';
import React from 'react';
import { DropdownMenu, DropdownMenuContent, DropdownMenuTriggerDots } from './dropdown';
import { cn } from '@/ui/cn';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {
  cohort?: CohortFragment;
}

export function Card({ cohort, children, ...props }: CardProps) {
  return (
    <div
      {...props}
      className={cn(
        'group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1',
        cohort ? 'pl-8' : '',
        props.className,
      )}
    >
      {children}
      {cohort && (
        <div
          className="absolute rounded-l-lg w-4 border-r border-neutral-6 shadow-sm inset-y-0 left-0"
          style={{ backgroundColor: cohort.sColorRgb }}
        />
      )}
    </div>
  );
}

export const CardMenu = ({ children }: { children: React.ReactNode }) => (
  <DropdownMenu modal={false}>
    <DropdownMenuTriggerDots />
    <DropdownMenuContent align="end">
      {children}
    </DropdownMenuContent>
  </DropdownMenu>
);
