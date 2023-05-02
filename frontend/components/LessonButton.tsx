import { usePermissions } from 'lib/data/use-permissions';
import {
  useBookLessonMutation,
  useCancelLessonMutation,
  ScheduleBasicFragment,
  ScheduleItemBasicFragment,
  useMyLessonsQuery,
} from 'lib/graphql/Schedule';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import React from 'react';
import classNames from 'classnames';
import { X as Cross } from 'react-feather';
import { SubmitButton } from './SubmitButton';
import { useQueryClient } from '@tanstack/react-query';
import { useAuth } from 'lib/data/use-auth';
import { formatCoupleName } from 'lib/format-name';
import { fullDateFormatter, shortDateFormatter } from 'lib/format-date';

type Props = {
  lesson: ScheduleItemBasicFragment;
  schedule: ScheduleBasicFragment;
  showTrainer?: boolean;
  showDate?: boolean;
};

export const LessonButton = ({ schedule, lesson, showTrainer, showDate }: Props) => {
  const { user } = useAuth();
  const perms = usePermissions();
  const [isOpen, setIsOpen] = React.useState(false);
  const queryClient = useQueryClient();

  const { mutateAsync: bookLesson, isLoading: isBooking } = useBookLessonMutation({
    onSuccess() {
      setIsOpen(false);
      queryClient.invalidateQueries(useMyLessonsQuery.getKey());
      queryClient.invalidateQueries(['ScheduleRange']);
    },
  });
  const { mutateAsync: cancelLesson, isLoading: isCanceling } = useCancelLessonMutation({
    onSuccess() {
      setIsOpen(false);
      queryClient.invalidateQueries(useMyLessonsQuery.getKey());
      queryClient.invalidateQueries(['ScheduleRange']);
    },
  });

  const canBook = perms.canSignUp(schedule, lesson);
  const canCancel = perms.canSignOut(schedule, lesson);

  const trainer = schedule.userByRTrener;
  const couple = lesson.paryByRiPartner;
  const isMyLesson =
    user?.id === couple?.userByPIdPartner?.id ||
    user?.id === couple?.userByPIdPartnerka?.id;

  const name = showTrainer
    ? `${trainer?.uJmeno} ${trainer?.uPrijmeni}`
    : formatCoupleName(couple);

  const [fromH = '0', fromM = '0'] = lesson.riOd.split(':');
  const [toH = '0', toM = '0'] = lesson.riDo.split(':');
  const duration =
    parseInt(toH) * 60 + parseInt(toM) - parseInt(fromH) * 60 - parseInt(fromM);

  const trigger = (
    <div
      className={classNames(
        'group flex gap-3 p-2.5 rounded-lg',
        'leading-4 text-sm tabular-nums',
        canCancel && 'cursor-pointer hover:bg-yellow-50',
        canBook && 'cursor-pointer hover:bg-green-100/80 bg-green-100 text-green-900',
        !showTrainer && isMyLesson && 'bg-yellow-100',
      )}
    >
      <div className="text-stone-600">
        {showDate ? shortDateFormatter.format(new Date(schedule.rDatum)) + ' ' : ''}
        {lesson.riOd.substring(0, 5)}
      </div>
      <div className="grow">
        {canBook ? 'VOLNÁ' : lesson.paryByRiPartner ? name : '-'}
      </div>
      <div className="text-stone-600">{duration}&apos;</div>
    </div>
  );

  return (
    <div className="relative">
      <PopoverPrimitive.Root open={isOpen} onOpenChange={setIsOpen}>
        <PopoverPrimitive.Trigger asChild>{trigger}</PopoverPrimitive.Trigger>
        <PopoverPrimitive.Content
          align="start"
          sideOffset={4}
          className={classNames(
            'z-20 radix-side-top:animate-slide-up radix-side-bottom:animate-slide-down',
            'w-48 rounded-lg p-4 shadow-md md:w-56',
            'bg-white dark:bg-gray-800',
          )}
        >
          <PopoverPrimitive.Arrow className="fill-current text-white dark:text-gray-800" />

          {canBook && (
            <SubmitButton
              loading={isBooking}
              onClick={() => bookLesson({ id: lesson.id })}
            >
              Přihlásit
            </SubmitButton>
          )}

          {canCancel && (
            <SubmitButton
              loading={isCanceling}
              onClick={() => cancelLesson({ id: lesson.id })}
            >
              Zrušit
            </SubmitButton>
          )}

          <PopoverPrimitive.Close
            className={classNames(
              'absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1',
              'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
            )}
          >
            <Cross className="h-4 w-4 text-gray-500 hover:text-gray-700 dark:text-gray-500 dark:hover:text-gray-400" />
          </PopoverPrimitive.Close>
        </PopoverPrimitive.Content>
      </PopoverPrimitive.Root>
    </div>
  );
};
