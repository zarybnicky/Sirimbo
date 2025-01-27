import React from 'react';
import Link, { LinkProps } from 'next/link';
import { buttonCls } from '@/ui/style';
import { cn } from '@/ui/cn';

export function ListItem(
  _n: number,
  item: {
    id: string;
    href: LinkProps['href'];
    title?: React.ReactNode;
    subtitle?: React.ReactNode;
    children?: React.ReactNode;
  },
  { currentId }: { currentId: string },
) {
  return (
    <Link
      key={item.id}
      href={item.href}
      className={buttonCls({ variant: currentId === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 mt-0 grid' })}
    >
      <div>{item.title}</div>
      <div className={cn('text-sm', currentId === item.id ? 'text-white' : 'text-neutral-11')}>
        {item.subtitle}
      </div>
      {item.children}
    </Link>
  );
}
