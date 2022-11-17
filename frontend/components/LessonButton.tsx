import { usePermissions } from 'lib/data/use-permissions';
import { ScheduleBasicFragment, ScheduleItemBasicFragment } from 'lib/graphql/Schedule';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import React from 'react';
import classNames from 'classnames';
import { X as Cross } from 'react-feather';

export const LessonButton = ({ schedule, lesson, showTrainer }: {
  lesson: ScheduleItemBasicFragment;
  schedule: ScheduleBasicFragment;
  showTrainer?: boolean;
}) => {
  const perms = usePermissions();

  const bookLesson = React.useCallback((id: string) => {
    /* if (!\Session::getZaplacenoPar()) {
     *   \Message::warning('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
     * } elseif ($lesson['ri_partner']) {
     *   \Message::warning('Lekce už je obsazená');
     * } else {
         if ri_partner is NULL, then
     *   self::query("UPDATE rozpis_item SET ri_partner='?' WHERE ri_id='?'", $uid, $rid);
     * } */
  }, []);

  const cancelLesson = React.useCallback((id: string) => {
    /* if ($lesson['ri_partner'] === null) {
     * } elseif ($par['p_id'] != $lesson['ri_partner']
     *           && !\Permissions::check('rozpis', P_OWNED, $data['r_trener'])
     * ) {
     *   \Message::warning('Nedostatečná oprávnění!');
     * } else {
     *   self::query("UPDATE rozpis_item SET ri_partner=NULL WHERE ri_id='?'", $rid)
     * } */
  }, []);

  const trainer = schedule.userByRTrener;
  const couple = lesson.paryByRiPartner;
  const man = couple?.userByPIdPartner;
  const woman = couple?.userByPIdPartnerka;
  let name = '';

  if (showTrainer) {
    name = `${trainer?.uJmeno} ${trainer?.uPrijmeni}`;
  } else if (!woman) {
    name = (['.', ',', '', undefined].includes(man?.uPrijmeni) ? man?.uJmeno
      : ['.', ',', '', undefined].includes(man?.uJmeno) ? man?.uPrijmeni
        : `${man?.uJmeno} ${man?.uPrijmeni}`) ?? '';
  } else {
    name = `${man?.uPrijmeni} - ${woman.uPrijmeni}`;
  }

  const [fromH = '0', fromM = '0'] = lesson.riOd.split(':');
  const [toH = '0', toM = '0'] = lesson.riDo.split(':');
  const duration = parseInt(toH) * 60 + parseInt(toM) - parseInt(fromH) * 60 - parseInt(fromM);

  return <>
    <div className="relative">
      <PopoverPrimitive.Root>
        <PopoverPrimitive.Trigger asChild>
          <div className={classNames(
            "flex gap-3 cursor-pointer p-2.5 rounded-lg",
            "leading-4 text-sm tabular-nums",
            "hover:bg-stone-200 radix-state-open:bg-stone-200",
          )}>
            <div className="text-stone-600">{lesson.riOd.substring(0, 5)}</div>
            <div className="grow">{perms.canSignUp(schedule, lesson) ? "VOLNÁ" : lesson.paryByRiPartner ? name : '-'}</div>
            <div className="text-stone-600">{duration}'</div>
          </div>
        </PopoverPrimitive.Trigger>
        <PopoverPrimitive.Content
          align="start"
          sideOffset={4}
          className={classNames(
            "z-20 radix-side-top:animate-slide-up radix-side-bottom:animate-slide-down",
            "w-48 rounded-lg p-4 shadow-md md:w-56",
            "bg-white dark:bg-gray-800"
          )}
        >
          <PopoverPrimitive.Arrow className="fill-current text-white dark:text-gray-800" />
          <h3 className="text-sm font-medium text-gray-900 dark:text-gray-100">
            Lekce
          </h3>

          {perms.canSignUp(schedule, lesson) ? (
            <button name="action" value="signup" className="button button-icon button-red button-sm py-0">+</button>
          ) : null}
          {perms.canSignOut(schedule, lesson) && (
            <button name="action" value="signout" className="button button-icon button-sm button-red py-0">&times;</button>
          )}

          <PopoverPrimitive.Close
            className={classNames(
              "absolute top-3.5 right-3.5 inline-flex items-center justify-center rounded-full p-1",
              "focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75"
            )}
          >
            <Cross className="h-4 w-4 text-gray-500 hover:text-gray-700 dark:text-gray-500 dark:hover:text-gray-400" />
          </PopoverPrimitive.Close>
        </PopoverPrimitive.Content>
      </PopoverPrimitive.Root>
    </div>
  </>;
};
