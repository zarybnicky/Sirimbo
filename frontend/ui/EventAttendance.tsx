import {
  type EventRegistrationAttendanceFragment,
  UpdateAttendanceDocument,
} from '@/graphql/Event';
import * as React from 'react';
import { useMutation } from 'urql';
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { cn } from '@/lib/cn';
import { FormError } from '@/ui/form';

export const attendanceIcons: Record<AttendanceType, LucideIcon> = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
};
const attendanceLabels: Record<AttendanceType, string> = {
  ATTENDED: 'Přítomen',
  UNKNOWN: 'Nezaznamenáno',
  NOT_EXCUSED: 'Nepřítomen',
};

export function EventAttendance({
  attendance,
  canEdit,
}: {
  attendance: EventRegistrationAttendanceFragment;
  canEdit: boolean;
}) {
  const [result, update] = useMutation(UpdateAttendanceDocument);
  const setStatus = useAsyncCallback(async (status: AttendanceType) => {
    await update({
      input: {
        eirId: attendance.id,
        note: attendance.attendanceNote,
        status: attendance.status === status ? 'UNKNOWN' : status,
      },
    });
  });

  if (!attendance.status) return null;

  return (
    <div className="text-center">
      {canEdit ? (
        <div className="inline-flex">
          {(['ATTENDED', 'NOT_EXCUSED'] as const).map((status) => (
            <button
              key={status}
              type="button"
              onClick={() => setStatus.execute(status)}
              disabled={setStatus.loading}
              aria-pressed={attendance.status === status}
              aria-label={attendanceLabels[status] + ': ' + (attendance.person?.name ?? '')}
              title={attendance.status === status ? 'Kliknutím zrušíte výběr' : attendanceLabels[status]}
              className={cn(
                'bg-neutral-1 text-neutral-11 hover:bg-neutral-3 px-2 py-1 text-sm first:rounded-l-xl last:rounded-r-xl',
                'border-y border-l last:border-r border-neutral-6 disabled:bg-neutral-2 disabled:text-neutral-8',
                'focus:relative focus:outline-hidden focus-visible:z-30 focus-visible:ring-3 focus-visible:ring-accent-10',
                attendance.status === status && (status === 'ATTENDED'
                  ? 'border-green-10 bg-green-9 hover:bg-green-8 text-white'
                  : 'border-danger-11 bg-danger-9 hover:bg-danger-10 text-white'),
              )}
            >
              {React.createElement(attendanceIcons[status])}
            </button>
          ))}
        </div>
      ) : (
        <span role="img" aria-label={attendanceLabels[attendance.status]}>
          {React.createElement(attendanceIcons[attendance.status], { className: 'mx-auto' })}
        </span>
      )}
      <FormError error={result.error} />
    </div>
  );
}
