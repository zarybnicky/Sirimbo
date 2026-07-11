'use client';

import { cn } from '@/lib/cn';
import { getHrefs, topMenu, type MenuStructItem } from '@/lib/use-menu';
import { getTenantUi } from '@/tenant/catalog';
import { AuthButton } from '@/ui/AuthButton';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { ChevronDown, Menu as MenuIcon, User as Account } from 'lucide-react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import React, { useMemo } from 'react';
import { useAtomValue } from 'jotai';
import { tenantIdAtom } from './state/auth';

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
  desktopLogo?: React.ReactNode;
  mobileLogo?: React.ReactNode;
  socialIcons?: React.ReactNode;
};

export function Header({
  isOpen,
  setIsOpen,
  showTopMenu,
  desktopLogo,
  mobileLogo,
  socialIcons,
}: Props) {
  const pathname = usePathname() ?? '';
  const auth = useAuth();
  const tenantId = useAtomValue(tenantIdAtom);
  const [isMounted, setIsMounted] = React.useState(false);

  const DesktopLogo = useMemo(() => getTenantUi(tenantId, 'DesktopLogo'), [tenantId]);
  const MobileLogo = useMemo(() => getTenantUi(tenantId, 'MobileLogo'), [tenantId]);
  const SocialIcons = useMemo(() => getTenantUi(tenantId, 'SocialIcons'), [tenantId]);

  React.useEffect(() => {
    setIsMounted(true);
  }, []);

  return (
    <div className="sticky z-20 top-0 inset-x-0 text-white bg-[#292524] shadow-lg print:hidden">
      <div className="lg:container lg:max-w-6xl relative">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
            {desktopLogo ?? <DesktopLogo />}
            {topMenu.map((x) => (
              <DesktopMenuItem key={x.title} item={x} pathname={pathname} />
            ))}
            <AuthButton />
            {socialIcons ?? <SocialIcons />}
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            onClick={() => setIsOpen(!isOpen)}
          >
            <MenuIcon />
          </button>

          <div className="grow flex items-center">
            {mobileLogo ?? <MobileLogo />}
          </div>

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

function DesktopMenuItem({
  item,
  pathname,
}: {
  item: MenuStructItem;
  pathname: string;
}) {
  const inPath = !!getHrefs(item).some((x) => {
    const y = typeof x === 'object' ? ('pathname' in x ? x.pathname : '') : x;
    if (!y) return false;
    return y === '/' ? pathname === '/' : pathname.startsWith(y);
  });

  const classes = cn(
    'flex gap-1 rounded-none transition-colors',
    'uppercase text-sm font-bold justify-center items-center',
    'hover:text-white hover:border-b-[3px] border-white data-[state=open]:border-b-[3px]',
    inPath
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide -mb-px'
      : 'text-[#f3f3f3] drop-shadow',
  );

  if (item.type === 'link') {
    return (
      <Link href={item.href} className={`${classes} ${item.className}`}>
        {item.title}
      </Link>
    );
  }

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={`block ${classes}`}>
        {item.title} <ChevronDown className="size-4" />
      </DropdownMenuTrigger>
      <DropdownMenuContent align="center">
        {item.children.map((child) => (
          <DropdownMenuLink key={JSON.stringify(child.href)} href={child.href}>
            {child.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
