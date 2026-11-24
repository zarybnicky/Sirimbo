'use client';

import { useAuth, type AccessRequirements } from '@/lib/auth';
import { cn } from '@/lib/cn';
import React, { useCallback } from 'react';

export interface TabProps extends AccessRequirements {
  id: string;
  title: React.ReactNode;
  children: React.ReactNode;
}

export function Tab({ children }: TabProps) {
  return children;
}

function collectTabs(children: React.ReactNode): TabProps[] {
  return React.Children.toArray(children).flatMap((child) => {
    if (!React.isValidElement<TabProps>(child)) return [];
    if (child.type === React.Fragment) return collectTabs(child.props.children);
    return child.type === Tab ? [child.props] : [];
  });
}

export interface TabMenuProps {
  className?: string;
  selected: string | null | undefined;
  onSelect: (x: string) => void;
  children?: React.ReactNode;
}

export function TabMenu({ className, children, selected, onSelect }: TabMenuProps) {
  const { canAccess } = useAuth();
  const tabs = collectTabs(children).filter(canAccess);
  const active = tabs.find((tab) => tab.id === selected) ?? tabs[0];
  if (!active) return null;

  return (
    <>
      {tabs.length > 1 && (
        <nav
          className={cn(
            'print:hidden border-b border-neutral-7 mb-2 flex space-x-4 max-w-full overflow-y-auto',
            className,
          )}
        >
          {tabs.map((tab) => (
            <TabButton
              key={tab.id}
              id={tab.id}
              title={tab.title}
              selected={active.id}
              onSelect={onSelect}
            />
          ))}
        </nav>
      )}

      <React.Fragment key={active.id}>{active.children}</React.Fragment>
    </>
  );
}

const TabButton = React.memo(function TabButton({
  id,
  title,
  onSelect,
  selected,
}: {
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
      }`}
    >
      {title}
    </button>
  );
});
