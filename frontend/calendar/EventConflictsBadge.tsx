import { useMemo } from 'react';
import { useAtomValue } from 'jotai';
import { AlertTriangle } from 'lucide-react';
import { cn } from '@/lib/cn';
import { calendarConflictsFor } from './state';

export function EventConflictsBadge({
  eventId,
  className,
}: {
  eventId: string;
  className?: string;
}) {
  const conflictsAtom = useMemo(() => calendarConflictsFor(eventId), [eventId]);
  const conflicts = useAtomValue(conflictsAtom);
  if (conflicts.length === 0) return null;

  return (
    <span className={cn('inline-flex items-center', className)}>
      <AlertTriangle className="size-4" />
    </span>
  );
}
