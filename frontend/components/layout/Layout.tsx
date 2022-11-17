import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

export const Layout: React.FC<{
  showTopMenu?: boolean;
  withBleeds?: boolean;
}> = ({ children, withBleeds = false, showTopMenu = false }) => {
  const [isOpen, setIsOpen] = React.useState(false);

  return <div className="h-screen w-full overflow-hidden">
    <Header {...{ isOpen, setIsOpen, showTopMenu }} />
    <div className="relative h-full flex">
      {!showTopMenu && <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />}
      <div className={`relative h-full overflow-y-auto ${withBleeds ? 'content' : ''}`}>
        {children}
        {showTopMenu && <Footer />}
      </div>
    </div>
  </div>;
};
