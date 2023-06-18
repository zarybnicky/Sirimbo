import { CohortFragment } from '@app/graphql/Cohorts';
import React from 'react';
import { MoreVertical } from 'lucide-react';
import { Dropdown, DropdownItem } from './Dropdown';
import { cn } from '@app/ui/cn';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {
  cohort?: CohortFragment;
  menu?: DropdownItem[];
}

export function Card({ menu, cohort, children, ...props }: CardProps) {
  return (
    <div
      {...props}
      className={cn(
        'group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1',
        cohort ? 'pl-8' : '',
        props.className,
      )}
    >
      {menu && menu.length > 0 && (
        <div className="absolute right-1 top-2">
          <Dropdown
            button={
              <button>
                <MoreVertical className="text-neutral-7 w-6 group:data-[state=open]:text-neutral-9 group-hover:text-neutral-8" />
              </button>
            }
            modal={false}
            align="end"
            options={menu}
          />
        </div>
      )}
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
