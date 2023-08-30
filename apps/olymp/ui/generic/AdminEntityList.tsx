import { fromSlugArray } from '@app/ui/slugify';
import React, { ReactNode } from 'react';
import { NextRouter } from 'next/router';
import classNames from 'classnames';
import Link from 'next/link';
import { buttonCls } from '../style';

export function RenderListItem(
  _n: number,
  item: {
    id: string;
    href: string;
    title?: ReactNode;
    subtitle?: ReactNode;
    children?: ReactNode;
  },
  { router }: { router: NextRouter },
) {
  const id = fromSlugArray(router.query.id);
  return (
    <Link
      key={item.id}
      href={item.href}
      className={buttonCls({ variant: id === item.id ? 'primary' : 'outline', display: 'none', className: 'pl-5 m-1 grid' })}
    >
      <div>{item.title}</div>
      <div className={classNames('text-sm', id === item.id ? 'text-white' : 'text-neutral-11')}>
        {item.subtitle}
      </div>
      {item.children}
    </Link>
  );
}
