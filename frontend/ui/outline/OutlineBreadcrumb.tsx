'use client';

import { cn } from '@/lib/cn';
import { nodeText } from '@/ui/outline/blocks';
import Link from 'next/link';
import React from 'react';

// Where a node sits in the outline. Each crumb zooms the outliner to that
// ancestor; `children` is the trailing crumb, for surfaces that have one.
export function OutlineBreadcrumb({
  ancestors,
  className,
  children,
}: {
  ancestors: readonly { id: string; content: unknown }[];
  className?: string;
  children?: React.ReactNode;
}) {
  return (
    <nav className={cn('flex flex-wrap items-center gap-1 text-neutral-11', className)}>
      <Link href="/outline" className="underline">
        Vše
      </Link>
      {ancestors.map((ancestor) => (
        <React.Fragment key={ancestor.id}>
          <span aria-hidden>›</span>
          <Link href={`/outline/${ancestor.id}`} className="underline">
            {nodeText(ancestor.content) || 'Bez názvu'}
          </Link>
        </React.Fragment>
      ))}
      {children && (
        <>
          <span aria-hidden>›</span>
          {children}
        </>
      )}
    </nav>
  );
}
