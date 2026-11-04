'use client';

import { type ReactNode, useState } from 'react';
import { ChevronDown, SlidersHorizontal } from 'lucide-react';
import { cn } from '@/lib/cn';
import { TextField } from '@/ui/fields/text';
import { buttonCls } from '@/ui/style';

export function ListHeader({
  pagination,
  search = '',
  onSearchChange,
  searchPlaceholder = 'Hledat…',
  activeFilterCount = 0,
  onReset,
  actions,
  collapsible = true,
  defaultFiltersOpen = true,
  children,
}: {
  pagination?: { total: number; page: number; limit: number };
  search?: string;
  onSearchChange?: (search: string) => void;
  searchPlaceholder?: string;
  activeFilterCount?: number;
  onReset?: () => void;
  actions?: ReactNode;
  collapsible?: boolean;
  defaultFiltersOpen?: boolean;
  children?: ReactNode;
}) {
  const [filtersOpen, setFiltersOpen] = useState(defaultFiltersOpen);
  return (
    <>
      <div className="flex flex-wrap items-center gap-2 py-2">
        {pagination && (
          <p className="whitespace-nowrap text-xs tabular-nums text-neutral-11">
            {pagination.total === 0
              ? '0 / 0'
              : `${(pagination.page - 1) * pagination.limit + 1}–${Math.min(pagination.page * pagination.limit, pagination.total)} / ${pagination.total}`}
          </p>
        )}
        {children && collapsible && (
          <button
            type="button"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => setFiltersOpen((open) => !open)}
          >
            <SlidersHorizontal className="size-3" aria-hidden />
            Filtry{activeFilterCount > 0 && ` (${activeFilterCount})`}
            <ChevronDown
              className={cn('size-3', filtersOpen && 'rotate-180')}
              aria-hidden
            />
          </button>
        )}
        {onReset && (
          <button
            type="button"
            className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
            onClick={onReset}
          >
            Zrušit filtry
          </button>
        )}
        {actions}
        {onSearchChange && (
          <TextField
            type="search"
            placeholder={searchPlaceholder}
            className="min-w-40 flex-1 sm:max-w-64 ml-auto"
            inputClassName="border-accent-6 bg-accent-2 py-1.5 text-sm text-neutral-12 placeholder:text-neutral-10 focus:border-accent-8 focus:ring-accent-8"
            value={search}
            onChange={(event) => onSearchChange(event.currentTarget.value)}
          />
        )}
      </div>
      {children && (
        <div
          className={cn(
            'mb-3 w-full grid-cols-1 gap-x-3 gap-y-2 sm:grid-cols-[max-content_minmax(0,1fr)]',
            !collapsible || filtersOpen ? 'grid' : 'hidden',
          )}
        >
          {children}
        </div>
      )}
    </>
  );
}
