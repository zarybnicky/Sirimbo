import { fromSlugArray } from '@app/ui/slugify';
import React, { ReactNode } from 'react';
import { NextRouter } from 'next/router';
import classNames from 'classnames';
import Link from 'next/link';

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
      className={classNames(
        'relative p-2 pl-5 mr-2 my-1 rounded-lg grid',
        id === item.id ? 'font-semibold bg-primary text-white shadow-md' : 'hover:bg-neutral-4',
      )}
    >
      <div>{item.title}</div>
      <div className={classNames('text-sm', id === item.id ? 'text-white' : 'text-neutral-11')}>
        {item.subtitle}
      </div>
      {item.children}
    </Link>
  );
}
