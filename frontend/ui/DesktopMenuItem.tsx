import { cn } from '@/lib/cn';
import { getHrefs, type MenuStructItem } from '@/lib/use-menu';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { ChevronDown } from 'lucide-react';
import Link from 'next/link';

export function DesktopMenuItem({
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
