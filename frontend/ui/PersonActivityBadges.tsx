'use client';

import { useState } from 'react';
import type { PersonLastActivityFragment } from '@/graphql/Person';
import { useTenantConfig } from '@/lib/auth';
import { cn } from '@/lib/cn';
import { dateTimeFormatter, formatDateOnly } from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';

const signals = [
  {
    key: 'webAt',
    label: 'Web',
    greenDays: 30,
    amberDays: 90,
  },
  {
    key: 'cardEntryAt',
    label: 'Karta',
    greenDays: 30,
    amberDays: 90,
  },
  {
    key: 'trainingAt',
    label: 'Trénink',
    greenDays: 30,
    amberDays: 90,
  },
  {
    key: 'competitionOn',
    label: 'Soutěž',
    greenDays: 90,
    amberDays: 365,
  },
] as const;

export function PersonActivityBadges({
  activity,
}: {
  activity: PersonLastActivityFragment;
}) {
  const { enableStarletImport } = useTenantConfig();
  const [now] = useState(Date.now);
  return (
    <div className="flex flex-wrap gap-1">
      {signals.map(({ key, label, greenDays, amberDays }) => {
        if (key === 'cardEntryAt' && !enableStarletImport) return null;
        const value = activity[key];
        const date = value
          ? new Date(key === 'competitionOn' ? `${value}T00:00:00` : value)
          : null;
        const ageDays = date ? (now - date.getTime()) / 86_400_000 : null;
        const recent = ageDays !== null && ageDays < greenDays;
        const middle = ageDays !== null && ageDays < amberDays;
        const status =
          ageDays === null
            ? null
            : recent
              ? `< ${greenDays} dní`
              : middle
                ? `${greenDays}–${amberDays} dní`
                : `≥ ${amberDays} dní`;
        const formatted =
          value && date
            ? key === 'competitionOn'
              ? formatDateOnly(value)
              : dateTimeFormatter.format(date)
            : 'Bez záznamu';
        const title = `${label}: ${formatted}${status ? ` (${status})` : ''}`;
        return (
          <Popover key={key}>
            <PopoverTrigger asChild>
              <button
                type="button"
                title={title}
                aria-label={title}
                className={cn(
                  'inline-flex items-center gap-1 rounded px-1.5 py-0.5 text-xs hover:bg-neutral-4 focus-visible:outline-2 focus-visible:outline-accent-9',
                  ageDays === null
                    ? 'bg-neutral-3 text-neutral-11'
                    : recent
                      ? 'bg-green-3 text-green-11'
                      : middle
                        ? 'bg-warning-11/10 text-warning-11'
                        : 'bg-danger-3 text-danger-11',
                )}
              >
                <span aria-hidden className="size-1.5 rounded-full bg-current" />
                {label}
              </button>
            </PopoverTrigger>
            <PopoverContent className="max-w-80 text-sm" side="top" align="start">
              <p className="pr-6 font-semibold">{label}</p>
              <p>{value ? <time dateTime={value}>{formatted}</time> : formatted}</p>
              {status && <p className="text-neutral-11">{status}</p>}
            </PopoverContent>
          </Popover>
        );
      })}
    </div>
  );
}
