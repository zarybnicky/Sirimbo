import React, { ReactNode } from 'react';
import Link from 'next/link';
import { buttonCls } from '../style';
import { cn } from '../cn';

export function RenderListItem(
  _n: number,
  item: {
    id: string;
    href: string;
    title?: ReactNode;
    subtitle?: ReactNode;
    children?: ReactNode;
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
