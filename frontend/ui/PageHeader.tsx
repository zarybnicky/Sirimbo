import React from 'react';
import { typographyCls } from '@/ui/style';
import Link, { LinkProps } from 'next/link';
import { ArrowLeft, ChevronRight } from 'lucide-react';
import { ActionGroup } from '@/ui/ActionGroup';
import { ResolvedAction } from '@/lib/actions';

type PageHeaderProps<Id extends string = string> = {
  variant?: NonNullable<Parameters<typeof typographyCls>[0]>['variant'];
  actions?: ResolvedAction<Id>[];
  primary?: Id | readonly Id[];
  primaryVariant?: 'primary' | 'outline';
  groups?: Record<'add', Id[]>;
  breadcrumbs?: { label: string; href: LinkProps['href'] }[];
  title: string;
  breadcrumbLabel?: string;
  subtitle?: React.ReactNode;
};

export function PageHeader<Id extends string = string>({
  breadcrumbs,
  title,
  breadcrumbLabel = title,
  subtitle,
  actions,
  primary,
  primaryVariant,
  groups,
  variant = 'heading',
}: PageHeaderProps<Id>) {
  const parent = breadcrumbs?.at(-1);

  return (
    <div className="my-4 min-w-0">
      {(parent?.href || !!actions?.length) && (
        <div className="mb-2 flex flex-wrap items-center justify-between gap-x-3 gap-y-1">
          {parent?.href && (
            <nav
              aria-label="Drobečková navigace"
              className="min-w-0 flex-1 text-sm text-neutral-11"
            >
              <Link
                href={parent.href}
                className="flex items-center gap-1.5 lg:hidden wrap-anywhere hover:text-neutral-12"
              >
                <ArrowLeft aria-hidden="true" className="size-3.5 shrink-0" />
                <span className="truncate" title={parent.label}>
                  {parent.label}
                </span>
              </Link>

              <ol className="hidden lg:flex flex-wrap items-baseline gap-1.5 wrap-anywhere">
                {breadcrumbs!.map((crumb, i) => (
                  <li
                    key={i}
                    className="inline-flex min-w-0 max-w-64 items-baseline gap-1.5"
                  >
                    {i > 0 && (
                      <ChevronRight
                        aria-hidden="true"
                        className="size-3 shrink-0 self-center text-neutral-9"
                      />
                    )}
                    <Link
                      href={crumb.href}
                      className="truncate text-neutral-11 hover:text-neutral-12"
                      title={crumb.label}
                    >
                      {crumb.label}
                    </Link>
                  </li>
                ))}
                <li className="inline-flex min-w-0 max-w-64 items-baseline gap-1.5">
                  <ChevronRight
                    aria-hidden="true"
                    className="size-3 shrink-0 self-center text-neutral-9"
                  />
                  <span aria-current="page" className="truncate" title={breadcrumbLabel}>
                    {breadcrumbLabel}
                  </span>
                </li>
              </ol>
            </nav>
          )}

          {!!actions?.length && (
            <ActionGroup
              className="ml-auto flex-wrap justify-end"
              primary={primary}
              primaryVariant={primaryVariant}
              groups={groups}
              actions={actions}
            />
          )}
        </div>
      )}

      <h1
        className={typographyCls({
          variant,
          spacing: 'default',
          className: 'wrap-anywhere',
        })}
      >
        {title}
      </h1>

      {subtitle && <div className="mt-1 text-sm text-accent-12">{subtitle}</div>}
    </div>
  );
}
