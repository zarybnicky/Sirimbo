import {
  type EventRegistrationAttendanceFragment,
  UpdateAttendanceDocument,
} from '@/graphql/Event';
import * as React from 'react';
import { useMutation } from 'urql';
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@/ui/form';
import { buttonCls, buttonGroupCls } from '@/ui/style';

export const attendanceIcons: Record<AttendanceType, LucideIcon> = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
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
    <div className="text-center flex flex-col justify-center items-center">
      {canEdit ? (
        <div className={buttonGroupCls()}>
          {(['ATTENDED', 'NOT_EXCUSED'] as const).map((status) => (
            <button
              key={status}
              type="button"
              onClick={() => setStatus.execute(status)}
              disabled={setStatus.loading}
              aria-pressed={attendance.status === status}
              className={buttonCls({
                display: 'compact',
                size: 'iconLg',
                variant:
                  attendance.status !== status
                    ? 'outline'
                    : status === 'ATTENDED'
                      ? 'success'
                      : 'danger',
              })}
            >
              {React.createElement(attendanceIcons[status], { strokeWidth: 1.5 })}
            </button>
          ))}
        </div>
      ) : (
        <span>
          {React.createElement(attendanceIcons[attendance.status], {
            className: 'mx-auto',
          })}
        </span>
      )}
      <FormError error={result.error} />
    </div>
  );
}
