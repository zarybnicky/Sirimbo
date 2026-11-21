import { cn } from '@/lib/cn';
import Image from '@/ui/Image';
import { buttonCls } from '@/ui/style';
import Link, { type LinkProps } from 'next/link';
import type { ReactNode } from 'react';

export function ListItem({
  href,
  title,
  subtitle,
  selected = false,
  imageUrl,
  avatarName,
  icon,
  colors,
  wrap = false,
}: {
  href: LinkProps['href'];
  title: ReactNode;
  subtitle?: ReactNode;
  selected?: boolean;
  imageUrl?: string | null;
  avatarName?: string | null;
  icon?: ReactNode;
  colors?: readonly string[];
  wrap?: boolean;
}) {
  const names = avatarName?.trim().split(/\s+/).filter(Boolean) ?? [];
  const initials = (names.length > 1 ? [names[0], names.at(-1)] : names)
    .map((name) => [...(name ?? '')][0] ?? '')
    .join('')
    .toUpperCase();

  return (
    <Link
      href={href}
      aria-current={selected ? 'page' : undefined}
      className={buttonCls({
        variant: selected ? 'primary' : 'outline',
        display: 'none',
        size: 'none',
        className: cn(
          'm-1 mt-0 flex items-center gap-2 rounded-lg border p-1.5 px-2 text-sm',
          selected && 'border-transparent',
          colors && 'pl-5',
        ),
      })}
    >
      {(imageUrl || initials || icon) && (
        <span
          aria-hidden="true"
          className="flex size-10 shrink-0 items-center justify-center overflow-hidden rounded-md bg-neutral-3 text-neutral-10"
        >
          {imageUrl ? (
            <Image
              src={imageUrl}
              alt=""
              width={40}
              height={40}
              className="size-10 object-cover"
            />
          ) : initials ? (
            <span className="text-lg text-neutral-11/70">{initials}</span>
          ) : (
            icon
          )}
        </span>
      )}
      <div className="min-w-0 flex-1">
        <div className={cn(!wrap && 'truncate')}>{title}</div>
        {subtitle && (
          <div
            className={cn(
              !wrap && 'truncate',
              selected ? 'text-accent-0' : 'text-neutral-11',
            )}
          >
            {subtitle}
          </div>
        )}
      </div>
      {colors?.length ? (
        <div
          aria-hidden="true"
          className="absolute inset-y-0 left-0 flex w-3 flex-col overflow-hidden rounded-l-lg shadow-xs"
        >
          {colors.map((color, index) => (
            <div key={`${color}-${index}`} className="min-h-0 flex-1" style={{ backgroundColor: color }} />
          ))}
        </div>
      ) : null}
    </Link>
  );
}
