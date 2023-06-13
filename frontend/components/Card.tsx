import { CohortFragment } from '@app/graphql/Cohorts';
import React from 'react';
import { MoreVertical } from 'lucide-react';
import { Dropdown, DropdownItem } from './Dropdown';
import { cn } from 'lib/utils';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {
  cohort?: CohortFragment;
  menu?: DropdownItem[];
}

export function Card({ menu, cohort, children, ...props }: CardProps) {
  return (
    <div
      {...props}
      className={cn(
        'group bg-white relative border border-stone-200 shadow-sm sm:rounded-lg p-3 mb-1',
        cohort ? 'pl-8' : '',
        props.className,
      )}
    >
      {menu && menu.length > 0 && (
        <div className="absolute right-1 top-2">
          <Dropdown
            button={
              <button>
                <MoreVertical className="text-stone-300 w-6 ui-open:text-stone-500 group-hover:text-stone-400 hover:text-stone-500" />
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
          className="absolute rounded-l-lg w-4 border-r border-stone-200 shadow-sm top-0 bottom-0 left-0"
          style={{ backgroundColor: cohort.sColorRgb }}
        />
      )}
    </div>
  );
}
