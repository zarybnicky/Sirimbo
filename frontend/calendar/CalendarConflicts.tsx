import React from 'react';
import { useAtomValue } from 'jotai';
import { AlertTriangle } from 'lucide-react';
import { cn } from '@/ui/cn';
import {
  calendarConflictsFor,
  type CalendarInstanceConflict,
} from './state';

const emptyNames = '';
const emptySummary = '';

function getConflictNames(conflicts: CalendarInstanceConflict[]): string {
  return conflicts
    .map((conflict) => conflict.personName ?? conflict.fallbackName)
    .join(', ');
}

function getConflictSummary(
  conflicts: CalendarInstanceConflict[],
  formatRange: (start: Date, end: Date) => string,
): string {
  return conflicts
    .map((conflict) => {
      const person = conflict.personName ?? conflict.fallbackName;
      const range = formatRange(
        new Date(conflict.otherSince),
        new Date(conflict.otherUntil),
      );
      return `${person}: ${conflict.otherEventName} (${range})`;
    })
    .join(' • ');
}

type CalendarConflictsProps = {
  instanceId: string | null | undefined;
  formatRange: (start: Date, end: Date) => string;
  className?: string;
};

export function CalendarConflicts({
  instanceId,
  formatRange,
  className,
}: CalendarConflictsProps) {
  const conflictsAtom = React.useMemo(
    () => calendarConflictsFor(instanceId),
    [instanceId],
  );
  const conflicts = useAtomValue(conflictsAtom);
  const hasConflicts = conflicts.length > 0;

  const conflictNames = React.useMemo(() => {
    if (!hasConflicts) return emptyNames;
    return getConflictNames(conflicts);
  }, [conflicts, hasConflicts]);

  const conflictSummary = React.useMemo(() => {
    if (!hasConflicts) return emptySummary;
    return getConflictSummary(conflicts, formatRange);
  }, [conflicts, formatRange, hasConflicts]);

  if (!hasConflicts) return null;

  return (
    <span
      className={cn('inline-flex items-center', className)}
      title={`Kolize – ${conflictSummary}`}
    >
      <span className="sr-only">Kolize: {conflictNames}</span>
      <AlertTriangle className="size-4" aria-hidden />
    </span>
  );
}
