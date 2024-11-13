import React, { useCallback, useMemo } from 'react';

export interface TabMenuProps {
  options: {
    id: string;
    title: React.ReactNode;
    contents: () => React.ReactNode;
  }[];
  selected: string | null | undefined;
  onSelect: (x: string) => void;
  children?: React.ReactNode;
}

export const TabMenu = React.memo(function TabMenu({ options, selected, onSelect }: TabMenuProps) {
  const active = useMemo(() => {
    return options.find(x => x.id === selected) || options[0] || {
      id: '',
      title: '',
      contents: () => null,
    };
  }, [options, selected]);

  return (
    <div className="">
      <nav className="border-b border-neutral-7 -mb-px flex space-x-4" aria-label="Tabs">
        {options.map((tab) => (
          <TabButton
            key={tab.id}
            id={tab.id}
            title={tab.title}
            selected={active.id}
            onSelect={onSelect}
          />
        ))}
      </nav>

      <div className="mt-2" key={active.id}>
        {useMemo(() => active.contents(), [active])}
      </div>
    </div>
  );
});

function TabButton({ id, title, onSelect, selected }: {
  id: string;
  title: React.ReactNode;
  selected: string;
  onSelect: (x: string) => void;
}) {
  const onClick = useCallback(() => onSelect(id), [id, onSelect]);
  return (
    <button
      type="button"
      key={id}
      onClick={onClick}
      aria-current={id === selected ? 'page' : undefined}
      className={`whitespace-nowrap py-2 px-1 border-b-2 font-medium text-sm inline-flex gap-1 ${
                id === selected
                  ? 'border-accent-9 text-accent-11'
                  : 'border-transparent text-neutral-11 hover:text-neutral-12 hover:border-neutral-8'
              }`
      }
    >
      {title}
    </button>
  );
}
