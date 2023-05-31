import {
  BookLessonDocument,
  CancelLessonDocument,
  ScheduleBasicFragment,
  ScheduleItemBasicFragment,
} from 'lib/graphql/Schedule';
import React from 'react';
import { Calendar, Clock, User, Users } from 'lucide-react';
import { SubmitButton } from './SubmitButton';
import { useAuth } from 'lib/data/use-auth';
import { formatCoupleName } from 'lib/format-name';
import { fullDateFormatter } from 'lib/format-date';
import { useMutation } from 'urql';

type LessonFormProps = {
  lesson: ScheduleItemBasicFragment;
  schedule: ScheduleBasicFragment;
  onSuccess?: () => void;
};

export function LessonForm({schedule, lesson, onSuccess}: LessonFormProps) {
  const { perms } = useAuth();
  const [{ fetching:bookFetching }, book] = useMutation(BookLessonDocument);
  const [{fetching: cancelFetching}, cancel] = useMutation(CancelLessonDocument);

  const canBook = perms.canSignUp(schedule, lesson);
  const canCancel = perms.canSignOut(schedule, lesson);

  const trainer = schedule.userByRTrener;
  const couple = lesson.paryByRiPartner;

  return (
    <div className="space-y-2">
      <div className="flex items-center gap-2">
        <Calendar className="w-6 h-6 text-red-500" />
        {fullDateFormatter.format(new Date(schedule.rDatum))}
      </div>

      <div className="flex items-center gap-2">
        <Clock className="w-6 h-6 text-red-500" />
        {lesson.riOd.substring(0, 5)} - {lesson.riDo.substring(0, 5)}
      </div>

      <div className="flex items-center gap-2">
        <User className="w-6 h-6 text-red-500" />
        {trainer?.uJmeno} {trainer?.uPrijmeni}
      </div>

      <div className="flex items-center gap-2">
        <Users className="w-6 h-6 text-red-500" />
        <span>{couple ? formatCoupleName(couple) : 'VOLNÁ'}</span>
      </div>

      {canBook && (
        <SubmitButton
          className="col-span-2"
          loading={bookFetching}
          onClick={async () => {
            await book({ id: lesson.id })
            onSuccess?.();
          }}
        >
          Přihlásit
        </SubmitButton>
      )}

      {canCancel && (
        <SubmitButton
          className="col-span-2"
          loading={cancelFetching}
          onClick={async () => {
            await cancel({ id: lesson.id });
            onSuccess?.();
          }}
        >
          Zrušit
        </SubmitButton>
      )}
    </div>
  );
}
