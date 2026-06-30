import React, { Fragment } from 'react';
import { typographyCls } from '@/ui/style';
import Link, { LinkProps } from 'next/link';
import { ArrowLeft, ChevronRight } from 'lucide-react';
import { ActionGroup } from '@/ui/ActionGroup';
import { ResolvedAction } from '@/lib/actions';

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

type PageHeaderProps<Id extends string = string> = {
  actions?: ResolvedAction<Id>[];
  primary?: Id | readonly Id[];
  groups?: Record<'add', Id[]>;
  breadcrumbs?: { label: string; href?: LinkProps['href'] }[];
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
}: PageHeaderProps<Id>) {
  const parent = breadcrumbs?.at(-2);

  return (
    <div className="my-4">
      {parent?.href && (
        <nav className="mb-2 text-sm text-neutral-7">
          <Link
            href={parent.href}
            className="flex items-center gap-1.5 lg:hidden hover:text-neutral-11"
          >
            <ArrowLeft className="size-3.5" />
            {parent.label}
          </Link>

          <div className="hidden lg:flex items-baseline gap-1.5">
            {breadcrumbs!.map((crumb, i) => (
              <Fragment key={i}>
                {i > 0 && <ChevronRight className="size-3 text-neutral-9" />}
                {crumb.href && i < breadcrumbs!.length - 1 ? (
                  <Link
                    href={crumb.href}
                    className="text-neutral-9 hover:text-neutral-12"
                  >
                    {crumb.label}
                  </Link>
                ) : (
                  <span className="text-neutral-11">{crumb.label}</span>
                )}
              </Fragment>
            ))}
          </div>
        </nav>
      )}

      <div className="flex flex-wrap-reverse justify-between gap-2 items-center">
        <div className="min-w-0">
          <h1 className={typographyCls({ variant: 'heading', spacing: 'default' })}>
            {title}
          </h1>
        </div>

        {actions && <ActionGroup className="ml-auto" primary={primary} groups={groups} actions={actions} />}
      </div>

      {subtitle && <p className="my-2 text-sm text-accent-12">{subtitle}</p>}
    </div>
  );
}
