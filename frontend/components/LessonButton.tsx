import {
  BookLessonDocument,
  CancelLessonDocument,
  ScheduleBasicFragment,
  ScheduleItemBasicFragment,
} from 'lib/graphql/Schedule';
import * as Popover from '@radix-ui/react-popover';
import React from 'react';
import classNames from 'classnames';
import { Calendar, Clock, User, Users, X as Cross } from 'lucide-react';
import { SubmitButton } from './SubmitButton';
import { useQueryClient } from '@tanstack/react-query';
import { useAuth } from 'lib/data/use-auth';
import { formatCoupleName } from 'lib/format-name';
import { fullDateFormatter, shortDateFormatter } from 'lib/format-date';
import { useGqlMutation } from 'lib/query';

type Props = {
  lesson: ScheduleItemBasicFragment;
  schedule: ScheduleBasicFragment;
  showTrainer?: boolean;
  showDate?: boolean;
};

export const LessonButton = ({ schedule, lesson, showTrainer, showDate }: Props) => {
  const { user, perms } = useAuth();
  const [isOpen, setIsOpen] = React.useState(false);

  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    setIsOpen(false);
    queryClient.invalidateQueries(['MyLessonsQuery']);
    queryClient.invalidateQueries(['ScheduleRange']);
  }, [queryClient]);

  const bookMutation = useGqlMutation(BookLessonDocument, { onSuccess });
  const cancelMutation = useGqlMutation(CancelLessonDocument, { onSuccess });

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
        'leading-4 text-sm tabular-nums cursor-pointer',
        canBook
          ? 'hover:bg-green-100/80 bg-green-100 text-green-900'
          : 'hover:bg-yellow-50',
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
      <Popover.Root open={isOpen} onOpenChange={setIsOpen}>
        <Popover.Trigger asChild>{trigger}</Popover.Trigger>
        <Popover.Content
          align="start"
          sideOffset={4}
          className={classNames(
            'z-20 radix-side-top:animate-slide-up radix-side-bottom:animate-slide-down',
            'w-48 rounded-lg p-4 shadow-md md:w-56',
            'bg-white dark:bg-stone-800',
          )}
        >
          <Popover.Arrow className="fill-current text-white dark:text-stone-800" />

          <div className="grid grid-cols-[1fr_5fr] gap-2">
            <Calendar className="text-red-500" />
            {fullDateFormatter.format(new Date(schedule.rDatum))}
            <Clock className="text-red-500" />
            <span>
              {lesson.riOd.substring(0, 5)} - {lesson.riDo.substring(0, 5)}
            </span>
            <User className="text-red-500" />
            <span>
              {trainer?.uJmeno} {trainer?.uPrijmeni}
            </span>

            <Users className="text-red-500" />
            <span>Obsazená: {couple ? 1 : 0}/1</span>

            <div />
            <span>{formatCoupleName(couple)}</span>

            {canBook && (
              <SubmitButton
                className="col-span-2"
                loading={bookMutation.isLoading}
                onClick={() => bookMutation.mutateAsync({ id: lesson.id })}
              >
                Přihlásit
              </SubmitButton>
            )}

            {canCancel && (
              <SubmitButton
                className="col-span-2"
                loading={cancelMutation.isLoading}
                onClick={() => cancelMutation.mutateAsync({ id: lesson.id })}
              >
                Zrušit
              </SubmitButton>
            )}
          </div>

          <Popover.Close
            className={classNames(
              'absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1',
              'focus:outline-none focus-visible:ring focus-visible:ring-red-500 focus-visible:ring-opacity-75',
            )}
          >
            <Cross className="h-4 w-4 text-stone-500 hover:text-stone-700 dark:text-stone-500 dark:hover:text-stone-400" />
          </Popover.Close>
        </Popover.Content>
      </Popover.Root>
    </div>
  );
};
