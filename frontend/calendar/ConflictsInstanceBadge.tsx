import React, { useMemo } from 'react';
import { useAtomValue } from 'jotai';
import { AlertTriangle } from 'lucide-react';
import { cn } from '@/lib/cn';
import { calendarConflictsFor } from './state';
import { shortTimeFormatter } from '@/ui/format';

export function ConflictsInstanceBadge({
  instanceId,
  className,
}: {
  instanceId: string;
  className?: string;
}) {
  const conflictsAtom = useMemo(() => calendarConflictsFor(instanceId), [instanceId]);
  const conflicts = useAtomValue(conflictsAtom);

  if (conflicts.length === 0) return null;
  const summary = conflicts
    .map((conflict) => {
      const range = shortTimeFormatter.formatRange(
        new Date(conflict.otherSince),
        new Date(conflict.otherUntil),
      );
      return `${conflict.personName}: ${range} ${conflict.otherEventName}`;
    })
    .join(' • ');

  return (
    <span
      className={cn('inline-flex items-center', className)}
      title={`Konflikt: ${summary}`}
    >
      <span className="sr-only">Konflikt: {summary}</span>
      <AlertTriangle className="size-4" aria-hidden />
    </span>
  );
}
