import { GlobalSearch } from '@/ui/GlobalSearch';
import { DialogTrigger } from '@/ui/dialog';
import { cn } from '@/lib/cn';
import { type MenuStructItem, topMenu } from '@/lib/use-menu';
import { getTenantUi } from '@/tenant/ui';
import { AuthButton } from '@/ui/AuthButton';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { useTenantId } from '@/lib/auth';
import { ChevronDown, Menu as MenuIcon, Search } from 'lucide-react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import React from 'react';

type Props = {
  homeHref: string;
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export function Header({ homeHref, isOpen, setIsOpen, showTopMenu }: Props) {
  const pathname = usePathname() ?? '';
  const tenantId = useTenantId();
  const { DesktopLogo, MobileLogo, SocialIcons } = getTenantUi(tenantId);

  return (
    <header className="sticky z-20 top-0 inset-x-0 text-white bg-[#292524] shadow-lg print:hidden">
      <div className="lg:container lg:max-w-6xl relative">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-12 md:min-h-16">
            <DesktopLogo href={homeHref} />
            {topMenu.map((x) => (
              <DesktopMenuItem key={x.title} item={x} pathname={pathname} />
            ))}
            <AuthButton />
            <SocialIcons />
          </div>
        )}

        <GlobalSearch mobile>
          <div className="col-start-1 row-start-1 flex items-stretch justify-between min-h-12 md:min-h-16 p-2 transition-opacity duration-200 motion-reduce:transition-none group-has-[[role=dialog][data-state=open]]:opacity-0 [&>:not(:last-child)]:transition-transform [&>:not(:last-child)]:duration-200 motion-reduce:[&>:not(:last-child)]:transition-none group-has-[[role=dialog][data-state=open]]:[&>:not(:last-child)]:-translate-x-3">
            <button
              className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
              onClick={() => setIsOpen((x) => !x)}
              aria-label={isOpen ? 'Zavřít menu' : 'Otevřít menu'}
              aria-controls="app-navigation"
              aria-expanded={isOpen}
            >
              <MenuIcon aria-hidden="true" />
            </button>

            <div className="grow flex items-center">
              <MobileLogo href={homeHref} />
            </div>

            <DialogTrigger.Plain
              aria-label="Hledat"
              className={buttonCls({
                className: 'm-1 hover:bg-white/10 active:bg-white/20',
                size: 'lg',
                variant: 'none',
              })}
            >
              <Search aria-hidden="true" />
            </DialogTrigger.Plain>
          </div>
        </GlobalSearch>
      </div>
    </header>
  );
}

function DesktopMenuItem({ item, pathname }: { item: MenuStructItem; pathname: string }) {
  const links = item.type === 'menu' ? item.children : [item];
  const inPath = links.some(({ href }) => {
    const [path] = href.split('?');
    if (!path) return false;
    return path === '/'
      ? pathname === '/'
      : pathname === path || pathname.startsWith(`${path}/`);
  });

  const classes = cn(
    'flex gap-1 rounded-none transition-colors',
    'uppercase text-sm font-bold justify-center items-center',
    'hover:text-white hover:border-b-[3px] border-white data-[state=open]:border-b-[3px]',
    inPath
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide -mb-px'
      : 'text-[#f3f3f3] drop-shadow-sm',
  );

  if (item.type !== 'menu') {
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
          <DropdownMenuLink key={child.href} href={child.href}>
            {child.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
