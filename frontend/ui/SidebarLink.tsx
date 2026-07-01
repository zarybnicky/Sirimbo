import { getHrefs, type MenuLink, type MenuStructItem } from '@/lib/use-menu';
import { cn } from '@/lib/cn';
import Link from 'next/link';
import React from 'react';

type SidebarLinkProps = {
  item: MenuLink;
  pathname: string;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
};

export function SidebarLink({ item, pathname, onClick }: SidebarLinkProps) {
  const inPath = !!getHrefs(item).some((x) => {
    const y = typeof x === 'object' ? ('pathname' in x ? x.pathname : '') : x;
    if (!y) return false;
    return y === '/' ? false : pathname.startsWith(y);
  });

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={cn(
        'rounded-2xl px-3 py-1.5',
        'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-neutral-11 text-white lg:bg-accent-10' : '',
        item.className,
      )}
    >
      {item.title}
    </Link>
  );
}

export function SidebarSection({
  item,
  pathname,
}: {
  item: MenuStructItem;
  pathname: string;
}) {
  return item.type === 'link' ? (
    <SidebarLink item={item} pathname={pathname} />
  ) : item.children.length > 0 ? (
    <>
      <div key={item.title} className="ml-5">
        <div className="font-bold text-xs uppercase grow mt-4">{item.title}</div>
      </div>
      <div className="list-none grid gap-0.5 pb-2">
        {item.children.map((y) => (
          <SidebarLink key={y.title} item={y} pathname={pathname} />
        ))}
      </div>
    </>
  ) : null;
}
