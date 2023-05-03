import * as PopoverPrimitive from '@radix-ui/react-popover';
import React from 'react';
import classNames from 'classnames';
import { X as Cross, Plus, Minus } from 'react-feather';
import { useAuth } from 'lib/data/use-auth';
import { formatCoupleName } from 'lib/format-name';
import {
  MyReservationFragment,
  useSetDesiredLessonsMutation,
} from 'lib/graphql/Reservation';
import { usePermissions } from 'lib/data/use-permissions';
import { useQueryClient } from '@tanstack/react-query';

export const ReservationButton = ({ item }: { item: MyReservationFragment }) => {
  const perms = usePermissions();
  const { couple } = useAuth();
  const [isOpen, setIsOpen] = React.useState(false);
  const queryClient = useQueryClient();
  const myLessons = item.myLessons || 0;
  const freeLessons = item.freeLessons || 0;

  const { isLoading, mutateAsync: modifyLessons } = useSetDesiredLessonsMutation({
    onSuccess() {
      queryClient.invalidateQueries(['ReservationRange']);
    },
  });
  const disableRemove = isLoading || myLessons <= 0;
  const disableAdd =
    isLoading ||
    freeLessons < 1 ||
    (item.nMaxPocetHod > 0 && myLessons >= item.nMaxPocetHod);

  const addLesson = React.useCallback(
    () =>
      modifyLessons({
        id: item.id,
        lessonCount: Math.min(myLessons + 1, myLessons + freeLessons),
      }),
    [freeLessons, modifyLessons, item, myLessons],
  );
  const removeLesson = React.useCallback(
    () =>
      modifyLessons({
        id: item.id,
        lessonCount: Math.max(myLessons - 1, 0),
      }),
    [modifyLessons, item, myLessons],
  );

  const canEdit = perms.canMakeReservation(item) || (item.myLessons && !item.nLock);
  const trigger = (
    <div
      className={classNames(
        'group flex gap-3 p-2.5 rounded-lg',
        'leading-4 text-sm tabular-nums',
        canEdit && 'cursor-pointer bg-yellow-100 border border-yellow-200 hover:bg-yellow-50',
      )}
    >
      <div>{formatCoupleName(couple)}</div>
      <div className="grow text-right">{myLessons}x</div>
    </div>
  );
  if (!canEdit) {
    return trigger;
  }

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

          <div className="flex gap-2">
            <button
              className="text-red-500 disabled:text-red-200"
              onClick={removeLesson}
              disabled={disableRemove}
            >
              <Minus className="w-5 h-5" />
            </button>
            <div className="text-xl tabular-nums">{myLessons}x</div>
            <button
              className="text-red-500 disabled:text-red-200"
              onClick={addLesson}
              disabled={disableAdd}
            >
              <Plus className="w-5 h-5" />
            </button>
          </div>

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
