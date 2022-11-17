import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

export const Layout: React.FC<{
  showTopMenu?: boolean;
}> = ({ children, showTopMenu = false }) => {
  const [isOpen, setIsOpen] = React.useState(false);

  return <div className="h-screen w-full overflow-hidden">
    <Header {...{ isOpen, setIsOpen, showTopMenu }} />
    <div className="relative h-full flex">
      {!showTopMenu && <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />}
      <div className="flex flex-1 flex-col overflow-y-auto">
        {children}
        {showTopMenu && <Footer />}
      </div>
    </div>
  </div>;
};
