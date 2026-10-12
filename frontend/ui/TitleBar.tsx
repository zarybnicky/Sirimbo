import React from 'react';
import { typographyCls } from '@/ui/style';
import Link, { LinkProps } from 'next/link';
import { ArrowLeft, ChevronRight } from 'lucide-react';
import { ActionGroup } from '@/ui/ActionGroup';
import { ResolvedAction } from '@/lib/actions';
import { cn } from '@/lib/cn';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
};

export function TitleBar({
  title,
  children,
  variant = 'heading',
  ...typography
}: TitleBarProps & Parameters<typeof typographyCls>[0]) {
  return (
    <div className="mb-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-baseline relative">
      <h1
        className={typographyCls({
          spacing: variant === 'heading' ? 'topLevel' : 'default',
          ...typography,
        })}
      >
        {title}
      </h1>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}

type PageHeaderProps<Id extends string = string> = Parameters<typeof typographyCls>[0] & {
  actions?: ResolvedAction<Id>[];
  primary?: Id | readonly Id[];
  groups?: Record<'add', Id[]>;
  breadcrumbs?: { label: string; href?: LinkProps['href']; truncate?: boolean }[];
  title: string;
  subtitle?: React.ReactNode;
};

export function PageHeader<Id extends string = string>({
  breadcrumbs,
  title,
  subtitle,
  actions,
  primary,
  groups,
  variant = 'heading',
}: PageHeaderProps<Id>) {
  const parent = breadcrumbs?.at(-2);

  return (
    <div className="my-4 min-w-0">
      {parent?.href && (
        <nav className="mb-2 text-sm text-neutral-11">
          <Link
            href={parent.href}
            className="flex items-center gap-1.5 lg:hidden wrap-anywhere hover:text-neutral-12"
          >
            <ArrowLeft className="size-3.5 shrink-0" />
            <span
              className={parent.truncate ? 'truncate' : undefined}
              title={parent.truncate ? parent.label : undefined}
            >
              {parent.label}
            </span>
          </Link>

          <div className="hidden lg:flex flex-wrap items-baseline gap-1.5 wrap-anywhere">
            {breadcrumbs!.map((crumb, i) => (
              <span
                key={i}
                className={cn(
                  'inline-flex min-w-0 items-baseline gap-1.5',
                  crumb.truncate && 'max-w-64',
                )}
              >
                {i > 0 && (
                  <ChevronRight
                    aria-hidden="true"
                    className="size-3 shrink-0 self-center text-neutral-9"
                  />
                )}
                {crumb.href && i < breadcrumbs!.length - 1 ? (
                  <Link
                    href={crumb.href}
                    className={cn(
                      'text-neutral-11 hover:text-neutral-12',
                      crumb.truncate && 'truncate',
                    )}
                    title={crumb.truncate ? crumb.label : undefined}
                  >
                    {crumb.label}
                  </Link>
                ) : (
                  <span
                    className={cn('text-neutral-11', crumb.truncate && 'truncate')}
                    title={crumb.truncate ? crumb.label : undefined}
                  >
                    {crumb.label}
                  </span>
                )}
              </span>
            ))}
          </div>
        </nav>
      )}

      <div className="flex flex-wrap-reverse justify-between gap-2 items-center">
        <div className="min-w-0">
          <h1
            className={typographyCls({
              variant,
              spacing: 'default',
              className: 'wrap-anywhere',
            })}
          >
            {title}
          </h1>
        </div>

        {actions && (
          <ActionGroup
            className="ml-auto"
            primary={primary}
            groups={groups}
            actions={actions}
          />
        )}
      </div>

      {subtitle && <p className="mt-1 text-sm text-accent-12">{subtitle}</p>}
    </div>
  );
}
