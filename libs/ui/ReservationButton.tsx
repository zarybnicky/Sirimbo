import * as PopoverPrimitive from '@radix-ui/react-popover';
import React from 'react';
import classNames from 'classnames';
import { X as Cross, Plus, Minus } from 'lucide-react';
import { useAuth } from '@app/ui/use-auth';
import { formatCoupleName } from '@app/ui/format-name';
import {
  MyReservationFragment,
  SetDesiredLessonsDocument,
} from '@app/graphql/Reservation';
import { useMutation } from 'urql';

export const ReservationButton = ({ item }: { item: MyReservationFragment }) => {
  const { perms, couple } = useAuth();
  const [isOpen, setIsOpen] = React.useState(false);
  const myLessons = item.myLessons || 0;
  const freeLessons = item.freeLessons || 0;

  const [{ fetching }, setMutation] = useMutation(SetDesiredLessonsDocument);
  const disableRemove = fetching || myLessons <= 0;
  const disableAdd =
    fetching ||
    freeLessons < 1 ||
    (item.nMaxPocetHod > 0 && myLessons >= item.nMaxPocetHod);

  const addLesson = React.useCallback(() => {
    const lessonCount = Math.min(myLessons + 1, myLessons + freeLessons);
    setMutation({ id: item.id, lessonCount });
  }, [freeLessons, setMutation, item, myLessons]);

  const removeLesson = React.useCallback(() => {
    const lessonCount = Math.max(myLessons - 1, 0);
    setMutation({ id: item.id, lessonCount });
  }, [setMutation, item, myLessons]);

  const canEdit = perms.canMakeReservation(item) || (item.myLessons && !item.nLock);
  const trigger =
    myLessons === 0 ? (!canEdit ? null : (
      <div
        className={classNames(
          'group flex justify-between gap-3 px-2.5 py-2 rounded-lg text-sm items-center',
          canEdit && 'cursor-pointer border border-accent-7 hover:bg-accent-4',
        )}
      >
        <div>Rezervovat</div>
        <Plus className="w-4 h-4" />
      </div>
    )) : (
      <div
        className={classNames(
          'group flex justify-between gap-3 px-2.5 py-2 rounded-lg leading-4 text-sm tabular-nums',
          canEdit && 'cursor-pointer bg-accent-4 border border-accent-7 hover:bg-accent-8',
        )}
      >
        <div>{formatCoupleName(couple)}</div>
        <div>{myLessons}x</div>
      </div>
    );
  if (!canEdit) {
    return trigger;
  }

  return (
    <PopoverPrimitive.Root open={isOpen} onOpenChange={setIsOpen}>
      <PopoverPrimitive.Trigger asChild>{trigger}</PopoverPrimitive.Trigger>
      <PopoverPrimitive.Portal>
        <PopoverPrimitive.Content
          align="start"
          sideOffset={4}
          className={classNames(
            'z-20 data-[side=top]:animate-slideUpAndFade data-[side=bottom]:animate-slideDownAndFade',
            'w-48 rounded-lg p-4 shadow-md md:w-56',
            'bg-neutral-1 text-neutral-12',
          )}
        >
          <PopoverPrimitive.Arrow className="fill-current text-neutral-1" />

          <div className="flex gap-2">
            <button
              className="text-accent-9 disabled:text-accent-7"
              onClick={removeLesson}
              disabled={disableRemove}
            >
              <Minus className="w-5 h-5" />
            </button>
            <div className="text-xl tabular-nums">{myLessons}x</div>
            <button
              className="text-accent-9 disabled:text-accent-7"
              onClick={addLesson}
              disabled={disableAdd}
            >
              <Plus className="w-5 h-5" />
            </button>
          </div>

          <PopoverPrimitive.Close
            className={classNames(
              'absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1',
              'focus:outline-none focus-visible:ring focus-visible:ring-red-500/75',
            )}
          >
            <Cross className="h-4 w-4 text-gray-500 hover:text-gray-700" />
          </PopoverPrimitive.Close>
        </PopoverPrimitive.Content>
      </PopoverPrimitive.Portal>
    </PopoverPrimitive.Root>
  );
};