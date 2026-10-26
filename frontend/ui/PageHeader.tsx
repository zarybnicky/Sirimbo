import React from 'react';
import { EditableImage } from '@/ui/EditableImage';
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
  imageUrl?: string | null;
  onEditImage?: () => void;
};

export function PageHeader<Id extends string = string>({
  breadcrumbs,
  title,
  breadcrumbLabel = title,
  subtitle,
  imageUrl,
  onEditImage,
  actions,
  primary,
  primaryVariant,
  groups,
  variant = 'heading',
}: PageHeaderProps<Id>) {
  const parent = breadcrumbs?.at(-1);

  return (
    <div className="my-4 min-w-0">
      <div className="flex flex-wrap-reverse items-center justify-between gap-x-3 gap-y-2 [&:has(>nav)>[data-page-title]]:basis-full">
        {breadcrumbs && parent?.href && (
          <nav className="min-w-0 flex-1 text-sm text-neutral-11">
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
              {breadcrumbs.map((crumb, i) => (
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

        <div
          data-page-title
          className="order-first flex min-w-0 max-w-full items-center gap-4"
        >
          <div className="min-w-0 flex-1">
            <h1
              className={typographyCls({
                variant,
                spacing: 'default',
                className: 'min-w-0 max-w-full wrap-anywhere',
              })}
            >
              {title}
            </h1>
            {(imageUrl || onEditImage) && subtitle && (
              <div className="mt-1 text-sm text-accent-12">{subtitle}</div>
            )}
          </div>
          <EditableImage
            src={imageUrl}
            onEdit={onEditImage}
            className="max-h-28 max-w-28 sm:max-h-32 sm:max-w-40"
            sizes="160px"
          />
        </div>

        {!!actions?.length && (
          <ActionGroup
            className="ml-auto max-w-full flex-wrap justify-end"
            primary={primary}
            primaryVariant={primaryVariant}
            groups={groups}
            actions={actions}
          />
        )}
      </div>

      {!imageUrl && !onEditImage && subtitle && (
        <div className="mt-1 text-sm text-accent-12">{subtitle}</div>
      )}
    </div>
  );
}
