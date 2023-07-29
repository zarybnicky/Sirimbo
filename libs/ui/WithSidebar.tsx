import React from 'react';
import classNames from 'classnames';

export const WithSidebar = ({ sidebar, children }: {
  sidebar: React.ReactNode;
  children?: React.ReactNode
}) => (
  <div className="col-full-width flex">
    <div
      className={classNames(
        'grow lg:flex-none lg:w-80 xl:w-96 sticky inset-y-0',
        'border-r lg:border-accent-6 lg:bg-accent-3 dark:lg:bg-accent-4',
        'overflow-y-auto scrollbar max-h-screen min-h-screen',
        children && 'hidden lg:flex lg:grow-0 flex-col',
      )}
    >
      {sidebar}
    </div>

    <div className={classNames('grow content relative', {
      'hidden lg:grid': !children,
      'min-h-0': children,
    })}>
      {children}
    </div>
  </div>
);
