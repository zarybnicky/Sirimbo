import * as React from 'react';
import Link from 'next/link';
import { MenuStructItem, getHrefs, useTopMenu, useSideMenu } from 'lib/data/use-menu';
import { useRouter } from 'next/router';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import classNames from 'classnames';
import { MobileSubmenu } from './MobileHeader';
import { Dropdown } from './Dropdown';

const MenuItem = ({ item: x }: { item: MenuStructItem }) => {
  const { pathname } = useRouter();
  const inPath = getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    return <Link href={x.href} passHref>
      <a className={classNames(
        "flex gap-2 items-center",
        "min-h-[48px] md:min-h-[64px] text-grey-500 font-bold rounded-none transition-colors",
        "small-caps",
        "hover:text-white hover:border-b-3 hover:border-white",
        inPath && 'text-white border-b-3 border-white'
      )}>
        {x.title}
      </a>
    </Link>;
  }
  const button = (
    <button className={classNames(
      "flex gap-2 items-center",
      "min-h-[48px] md:min-h-[64px] text-grey-500 font-bold rounded-none transition-colors",
      "small-caps",
      "hover:text-white hover:border-b-3 hover:border-white",
      inPath && 'text-white border-b-3 border-white'
    )}>
      {x.title}
      <KeyboardArrowDownIcon className="w-4.5 h-4.5" />
    </button>
  );
  return <Dropdown align="center" button={button} options={x.children} />
};

export const DesktopMenu = () => {
  const menu = useTopMenu();
  return <>{menu.map(x => <MenuItem key={x.title} item={x} />)}</>;
}

export const DesktopSidemenu = () => {
  const menu = useSideMenu();
  if (menu.length < 1) {
    return null;
  }
  return (
    <nav className={classNames(
      'hidden md:block',
      `w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 2xl:w-72 3xl:w-80 z-30 flex flex-col h-full max-h-screen min-h-screen flex-none transform overflow-y-auto border-r border-slate-200 bg-muted pb-10 sm:pb-0 lg:relative lg:z-auto lg:translate-x-0 lg:bg-gray-50`
    )}>
      {menu.map(x => <MobileSubmenu key={x.title} item={x} />)}
    </nav>
  );
}
