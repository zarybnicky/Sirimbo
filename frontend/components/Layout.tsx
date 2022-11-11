import * as React from 'react';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { Footer } from './Footer';
import { DesktopSidemenu } from './DesktopMenu';

export const Layout: React.FC = ({ children }) => {
  return <div className="h-screen w-full overflow-hidden">
    <DesktopHeader />
    <MobileHeader />
    <div className="relative h-full flex">
      <DesktopSidemenu />
      <div className="flex flex-1 flex-col overflow-y-auto">
        {children}
        <Footer />
      </div>
    </div>
  </div>;
};
