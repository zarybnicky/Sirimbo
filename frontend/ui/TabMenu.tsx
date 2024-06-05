import React from 'react';

export interface TabMenuProps {
  options: { id: string; label: React.ReactNode }[];
  selected: string | undefined;
  onSelect: (x: string) => void;
}

export const TabMenu = React.memo(function TabMenu({
  options,
  selected,
  onSelect,
}: TabMenuProps) {
  return (
    <div className="border-b border-neutral-7">
      <nav className="-mb-px flex space-x-4" aria-label="Tabs">
        {options.map((tab) => (
          <button
            type="button"
            key={tab.id}
            onClick={() => onSelect(tab.id)}
            aria-current={tab.id === selected ? 'page' : undefined}
            className={`whitespace-nowrap py-2 px-1 border-b-2 font-medium text-sm inline-flex gap-1 ${
                tab.id === selected
                  ? 'border-accent-9 text-accent-11'
                  : 'border-transparent text-neutral-11 hover:text-neutral-12 hover:border-neutral-8'
              }`
            }
          >
            {tab.label}
          </button>
        ))}
      </nav>
    </div>
  );
});
