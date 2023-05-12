import classNames from 'classnames';
import { CohortFragment } from 'lib/graphql/Cohorts';
import React from 'react';
import { Dropdown, DropdownItem } from './Dropdown';

interface CardProps extends React.HTMLAttributes<HTMLDivElement> {
  cohort?: CohortFragment;
  menu?: DropdownItem[];
}

export function Card({ menu, cohort, children, ...props }: CardProps) {
  return (
    <div
      {...props}
      className={classNames(
        'bg-white relative border border-stone-200 shadow-sm sm:rounded-lg p-3 mb-2',
        cohort ? 'pl-8' : '',
        props.className,
      )}
    >
      {menu && menu.length > 0 && (
        <Dropdown className="absolute right-1 top-2" align="end" options={menu} />
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
