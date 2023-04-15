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
    /* <div>
     <div className="sm:hidden">
          <label htmlFor="tabs" className="sr-only">Vyberte panel</label>
          <select
          name="tabs"
          className="block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
          onChange={(e) => onSelect(e.currentTarget.value)}
          defaultValue={selected}
          >
          {options.map((tab) => <option key={tab.id} value={tab.id}>{tab.label}</option>)}
          </select>
          </div>
    <div className="hidden sm:block"> */
    <div className="border-b border-gray-200">
      <nav className="-mb-px flex space-x-8" aria-label="Tabs">
        {options.map((tab) => (
          <a
            key={tab.id}
            href="#"
            onClick={() => onSelect(tab.id)}
            aria-current={tab.id === selected ? 'page' : undefined}
            className={classNames(
              tab.id === selected
                ? 'border-red-500 text-red-600'
                : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300',
              'whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm',
            )}
          >
            {tab.label}
          </a>
        ))}
      </nav>
    </div>
    /* </div> </div > */
  );
});
