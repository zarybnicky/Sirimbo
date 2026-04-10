import React, { Fragment } from 'react';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@/ui/style';
import Link, { LinkProps } from 'next/link';
import { ArrowLeft, ChevronRight } from 'lucide-react';
import { ActionGroup } from '@/ui/ActionGroup';
import { type ResolvedActions } from '@/lib/actions';

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
      {title && variant === 'heading' && <NextSeo title={title} />}
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

type PageHeaderProps<TItem extends object = object> = {
  breadcrumbs?: { label: string; href?: LinkProps['href'] }[];
  title: string;
  subtitle?: React.ReactNode;
  actions?: ResolvedActions<TItem>;
};

type TitleActionsRowProps<TItem extends object = object> = {
  title?: React.ReactNode | null;
  actions?: ResolvedActions<TItem>;
};

export function TitleActionsRow<TItem extends object = object>({
  title,
  actions,
  variant = 'heading',
  ...typography
}: TitleActionsRowProps<TItem> &
  Omit<Partial<NonNullable<Parameters<typeof typographyCls>[0]>>, 'class'>) {
  return (
    <div className="flex justify-between gap-4 items-baseline">
      <div className="min-w-0">
        <h1
          className={typographyCls({
            variant,
            spacing: variant === 'heading' ? 'topLevel' : undefined,
            ...typography,
          })}
        >
          {title}
        </h1>
      </div>

      {actions && <ActionGroup actions={actions} />}
    </div>
  );
}

export function PageHeader<TItem extends object = object>({
  breadcrumbs,
  title,
  subtitle,
  actions,
}: PageHeaderProps<TItem>) {
  const parent = breadcrumbs?.at(-2);

  return (
    <div className="mb-4">
      {title && <NextSeo title={title} />}

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

      <TitleActionsRow
        variant="heading"
        spacing="default"
        title={title}
        actions={actions}
      />
      {subtitle && <p className="my-2 text-sm text-accent-12">{subtitle}</p>}
    </div>
  );
}
