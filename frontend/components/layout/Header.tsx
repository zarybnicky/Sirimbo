import { type MenuStructItem, getHrefs, topMenu } from '@/lib/use-menu';
import { DesktopLogo, MobileLogo, SocialIcons } from '@/tenant/current/ui';
import { AuthButton } from '@/ui/AuthButton';
import { cn } from '@/ui/cn';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { User as Account, ChevronDown, Menu as MenuIcon } from 'lucide-react';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Header = ({ isOpen, setIsOpen, showTopMenu }: Props) => {
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
            <DesktopLogo />
            {topMenu.map((x) => (
              <DesktopMenuItem key={x.title} item={x} />
            ))}
            <AuthButton />
            <SocialIcons />
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
            <MobileLogo />
          </div>

          <Link
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            href={(auth.user && isMounted) ? '/profil' : '/login'}
          >
            <Account />
          </Link>
        </div>
      </div>
    </div>
  );
};


const DesktopMenuItem = ({ item: x }: { item: MenuStructItem }) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find((y) =>
    y === '/' ? pathname === '/' : pathname.startsWith(y),
  );

  const classes = cn(
    'flex gap-1 rounded-none transition-colors',
    'uppercase text-sm font-bold justify-center items-center',
    'hover:text-white hover:border-b-[3px] border-white data-[state=open]:border-b-[3px]',
    inPath
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide -mb-px'
      : 'text-neutral-3 drop-shadow',
  );
  if (x.type === 'link') {
    return (
      <Link href={x.href} className={classes + ` ${x.className}`}>
        {x.title}
      </Link>
    );
  }
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={'block ' + classes}>
        {x.title} <ChevronDown className="size-4" />
      </DropdownMenuTrigger>
      <DropdownMenuContent align="center">
        {x.children.map((item) => (
          <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
            {item.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
