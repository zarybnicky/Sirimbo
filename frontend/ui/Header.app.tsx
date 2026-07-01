'use client';

import { topMenu } from '@/lib/use-menu';
import { AuthButton } from '@/ui/AuthButton';
import { DesktopMenuItem } from '@/ui/DesktopMenuItem';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { Menu as MenuIcon, User as Account } from 'lucide-react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import React from 'react';

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu: boolean;
  desktopLogo: React.ReactNode;
  mobileLogo: React.ReactNode;
  socialIcons: React.ReactNode;
};

export function Header({
  isOpen,
  setIsOpen,
  showTopMenu,
  desktopLogo,
  mobileLogo,
  socialIcons,
}: Props) {
  const pathname = usePathname() || '/';
  const auth = useAuth();
  const [isMounted, setIsMounted] = React.useState(false);

  React.useEffect(() => {
    setIsMounted(true);
  }, []);

  return (
    <div className="sticky z-20 top-0 inset-x-0 text-white bg-[#292524] shadow-lg">
      <div className="lg:container lg:max-w-6xl relative">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
            {desktopLogo}
            {topMenu.map((item) => (
              <DesktopMenuItem key={item.title} item={item} pathname={pathname} />
            ))}
            <AuthButton />
            {socialIcons}
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button
            type="button"
            aria-label="Otevřít menu"
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            onClick={() => setIsOpen(!isOpen)}
          >
            <MenuIcon />
          </button>

          <div className="grow flex items-center">{mobileLogo}</div>

          <Link
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            href={auth.user && isMounted ? '/profil' : '/login'}
          >
            <Account />
          </Link>
        </div>
      </div>
    </div>
  );
}
