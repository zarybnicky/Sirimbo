import classNames from 'classnames';
import React from 'react';

export const TabMenu = React.memo(function TabMenu({
  options,
  selected,
  onSelect,
}: {
  options: { id: string; label: React.ReactNode }[];
  selected: string;
  onSelect: (x: string) => void;
}) {
  return (
    <div className="border-b border-neutral-7">
      <nav className="-mb-px flex space-x-6" aria-label="Tabs">
        {options.map((tab) => (
          <a
            key={tab.id}
            href="#"
            onClick={() => onSelect(tab.id)}
            aria-current={tab.id === selected ? 'page' : undefined}
            className={classNames(
              tab.id === selected
                ? 'border-accent-9 text-accent-11'
                : 'border-transparent text-neutral-11 hover:text-neutral-12 hover:border-neutral-8',
              'whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm',
            )}
          >
            {tab.label}
          </a>
        ))}
      </nav>
    </div>
  );
});
