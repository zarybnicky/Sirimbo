import classNames from 'classnames';
import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

export const Layout: React.FC<{
  showTopMenu?: boolean;
  list?: React.ReactNode;
  isDetail?: boolean;
}> = ({ children, showTopMenu, list, isDetail }) => {
  const [isOpen, setIsOpen] = React.useState(false);

  return <div className="h-screen flex flex-col w-full relative overflow-hidden">
    <Header {...{ isOpen, setIsOpen, showTopMenu }} />
    <div className="relative flex overflow-hidden">
      {!showTopMenu && <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />}
      {list ? <>
        <div className={classNames(
          isDetail ? 'hidden lg:flex flex-col' : 'max-h-screen min-h-screen w-full',
        )}>
          {list}
        </div>
        <div className={classNames(
          "scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50",
          isDetail ? "grow px-4 py-12 content min-h-0 overflow-y-auto" : 'hidden lg:flex',
        )}>
          {children}
        </div>
      </> : (
        <div className={classNames(
          "relative h-full grow overflow-y-auto content",
          "scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/20 hover:scrollbar-thumb-stone-800/50",
        )}>
          {children}
          {showTopMenu && <Footer />}
        </div>
      )}
    </div>
  </div>;
};
