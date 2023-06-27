import { ScheduleBasicFragment, ScheduleItemBasicFragment } from '@app/graphql/Schedule';
import * as Collapsible from '@radix-ui/react-collapsible';
import React from 'react';
import classNames from 'classnames';
import { useAuth } from '@app/ui/use-auth';
import { formatCoupleName } from '@app/ui/format-name';
import { shortDateFormatter } from '@app/ui/format-date';
import { LessonForm } from './LessonForm';

type Props = {
  lesson: ScheduleItemBasicFragment;
  schedule: ScheduleBasicFragment;
  showTrainer?: boolean;
  showDate?: boolean;
  alwaysExpanded?: boolean;
};

export const LessonButton = ({ schedule, lesson, showTrainer, showDate, alwaysExpanded }: Props) => {
  const { user, perms } = useAuth();
  const [isOpen, setIsOpen] = React.useState(!!alwaysExpanded);

  const canBook = perms.canSignUp(schedule, lesson);

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
          : 'hover:bg-accent-4',
        !showTrainer && isMyLesson && 'bg-accent-5',
      )}
    >
      <div className="text-neutral-11">
        {showDate ? shortDateFormatter.format(new Date(schedule.rDatum)) + ' ' : ''}
        {lesson.riOd.substring(0, 5)}
      </div>
      <div className="grow">
        {canBook ? 'VOLNÁ' : lesson.paryByRiPartner ? name : '-'}
      </div>
      <div className="text-neutral-11">{duration}&apos;</div>
    </div>
  );

  return (
    <Collapsible.Root open={alwaysExpanded || isOpen} onOpenChange={setIsOpen}>
      <Collapsible.Trigger asChild>{trigger}</Collapsible.Trigger>
      <Collapsible.Content className="CollapsibleContent">
        <div className="px-4 py-3 mb-2 bg-neutral-2 rounded-md">
          <LessonForm
            lesson={lesson}
            schedule={schedule}
            onSuccess={() => setIsOpen(false)}
          />
        </div>
      </Collapsible.Content>
    </Collapsible.Root>
  );
};
