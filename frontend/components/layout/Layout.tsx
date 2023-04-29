import classNames from 'classnames';
import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

type Props = {
  showTopMenu?: boolean;
  list?: React.ReactNode;
  isDetail?: boolean;
  children?: React.ReactNode;
};

export function Layout({ children, showTopMenu, list, isDetail }: Props) {
  const [isOpen, setIsOpen] = React.useState(false);

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />
      <div className="relative grow flex overflow-hidden">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />
        {list ? (
          <>
            <div
              className={classNames(
                'grow',
                isDetail
                  ? 'hidden lg:flex lg:grow-0 flex-col'
                  : 'max-h-screen min-h-screen w-full',
              )}
            >
              {list}
            </div>
            <div
              className={classNames(
                isDetail ? 'grow content min-h-0 overflow-y-auto' : 'hidden lg:flex',
                'scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50',
              )}
            >
              {children}
            </div>
          </>
        ) : (
          <div
            className={classNames(
              'relative h-full grow overflow-y-auto content',
              'scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50',
            )}
          >
            {children}
            {showTopMenu && <Footer />}
          </div>
        )}
      </div>
    </div>
  );
}
