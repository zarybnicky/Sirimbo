import React from 'react';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@/ui/style';
import { Fragment } from 'react';
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

type PageHeaderProps<TItem> = {
  breadcrumbs?: { label: string; href?: LinkProps['href'] }[];
  title: string;
  subtitle?: React.ReactNode;
  actions?: ResolvedActions<TItem>;
};

export function PageHeader<TItem>({
  breadcrumbs,
  title,
  subtitle,
  actions,
  variant = 'heading',
  ...typography
}: PageHeaderProps<TItem> & Parameters<typeof typographyCls>[0]) {
  const parent = breadcrumbs?.at(-2);

  return (
    <div className="mb-4">
      <NextSeo title={title} />

      {parent?.href && (
        <nav className="mb-1.5 text-sm text-neutral-7">
          <Link
            href={parent.href}
            className="flex items-center gap-1.5 lg:hidden hover:text-neutral-11"
          >
            <ArrowLeft className="size-3.5" />
            {parent.label}
          </Link>

          <div className="hidden lg:flex items-center gap-1.5">
            {breadcrumbs!.map((crumb, i) => (
              <Fragment key={i}>
                {i > 0 && <ChevronRight className="size-3 text-neutral-3" />}
                {crumb.href && i < breadcrumbs!.length - 1 ? (
                  <Link href={crumb.href} className="hover:text-neutral-11">
                    {crumb.label}
                  </Link>
                ) : (
                  <span className="text-neutral-7">{crumb.label}</span>
                )}
              </Fragment>
            ))}
          </div>
        </nav>
      )}

      <div className="flex justify-between items-baseline gap-4">
        <div className="min-w-0">
          <h1
            className={typographyCls({
              spacing: variant === 'heading' ? 'topLevel' : 'default',
              ...typography,
            })}
          >
            {title}
          </h1>
          {subtitle && <p className="mt-0.5 text-sm text-accent-12">{subtitle}</p>}
        </div>

        {actions && <ActionGroup actions={actions} />}
      </div>
    </div>
  );
}
