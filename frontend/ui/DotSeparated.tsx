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
      className={cn(
        'inline-flex max-w-full flex-wrap items-baseline gap-x-4 gap-y-0.75 overflow-clip [overflow-clip-margin:4px] wrap-anywhere',
        className,
      )}
      {...props}
    >
      {items.map((item, index) => (
        <span
          key={isValidElement(item) ? item.key : index}
          className="relative inline-flex min-w-0 items-baseline"
        >
          {index > 0 && (
            <span aria-hidden="true" className="absolute -left-4 w-4 text-center opacity-60">
              ·
            </span>
          )}
          {item}
        </span>
      ))}
    </span>
  );
}
