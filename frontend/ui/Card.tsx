import { cn } from '@/ui/cn';
import React from 'react';

export function Card({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      {...props}
      className={cn(
        'group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1',
        className,
      )}
    >
      {children}
    </div>
  );
}
