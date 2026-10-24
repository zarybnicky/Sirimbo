import { Children, isValidElement, type ComponentPropsWithoutRef } from 'react';
import { cn } from '@/lib/cn';

export function DotSeparated({
  children,
  className,
  ...props
}: ComponentPropsWithoutRef<'span'>) {
  const items = Children.toArray(children).filter(
    (item) => typeof item !== 'string' || item.trim() !== '',
  );
  if (items.length === 0) return null;

  return (
    <span
      className={cn('inline-flex flex-wrap items-baseline gap-y-0.75', className)}
      {...props}
    >
      {items.map((item, index) => (
        <span
          key={isValidElement(item) ? item.key : index}
          className="inline-flex min-w-0 items-baseline"
        >
          {index > 0 && (
            <span aria-hidden="true" className="mx-1.5 opacity-60">
              ·
            </span>
          )}
          {item}
        </span>
      ))}
    </span>
  );
}
