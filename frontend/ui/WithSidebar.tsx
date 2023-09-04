import React from 'react';
import { cn } from './cn';

export const WithSidebar = ({ sidebar, children, className }: {
  sidebar: React.ReactNode;
  children?: React.ReactNode;
  className?: string;
}) => (
  <div className="col-full-width flex">
    <div
      className={cn(
        'grow lg:flex-none lg:w-80 xl:w-96 sticky inset-y-0',
        'border-r lg:border-accent-6 lg:bg-accent-1 px-1',
        'overflow-y-auto scrollbar max-h-screen min-h-screen',
        children && 'hidden lg:flex lg:grow-0 flex-col',
      )}
    >
      {sidebar}
    </div>

    <div className={cn('grow content relative content-start', className, {
      'hidden lg:grid': !children,
      'min-h-0': children,
    })}>
      {children}
    </div>
  </div>
);
