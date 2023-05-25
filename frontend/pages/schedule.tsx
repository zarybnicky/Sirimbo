import * as React from 'react';
import { ReservationRangeDocument } from 'lib/graphql/Reservation';
import { ScheduleFragment, ScheduleRangeDocument } from 'lib/graphql/Schedule';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { formatWeekDay } from 'lib/format-date';
import { ScheduleItem } from 'components/ScheduleItem';
import { ReservationItem } from 'components/ReservationItem';
import {
  getCurrentMonday,
  mondayToWeekRange,
  mondayToYearRange,
  WeekPicker,
} from 'components/WeekPicker';
import { Item } from 'components/layout/Item';
import type { NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';

const Page: NextPageWithLayout = () => {
  const [startDate, setStartDate] = React.useState(getCurrentMonday);

  const { data: schedules } = useGqlQuery(ScheduleRangeDocument, mondayToWeekRange(startDate));
  const { data: reservations } = useGqlQuery(ReservationRangeDocument, mondayToYearRange(startDate));

  const scheduleByDay = React.useMemo(() => {
    const obj: { [date: string]: ScheduleFragment[] } = {};
    schedules?.schedulesForRange?.nodes?.forEach((item) => {
      const arr = obj[item.rDatum] || [];
      arr.push(item);
      obj[item.rDatum] = arr;
    });
    return obj;
  }, [schedules]);

  return (
    <Item className="col-full-width bg-stone-100">
      <WeekPicker title="Tréninky" startDate={startDate} onChange={setStartDate} />

      {!reservations?.reservationsForRange?.nodes?.length &&
        !schedules?.schedulesForRange?.nodes?.length && (
          <div className="border p-2 bg-red-50">Žádné tréninky pro tento týden</div>
        )}
      {!!reservations?.reservationsForRange?.nodes?.length && (
        <>
          <div className="text-xl tracking-wide text-stone-700 mb-2">
            Nabídky tréninků
          </div>
          <div className="flex justify-start flex-wrap gap-4 ml-2 pl-5 border-l-4 border-red-400">
            {reservations?.reservationsForRange?.nodes.map((item, i) => (
              <ReservationItem key={i} item={item} />
            ))}
          </div>
        </>
      )}

      {Object.entries(scheduleByDay).map(([date, items], i) => (
        <React.Fragment key={i}>
          <div className="text-2xl tracking-wide text-stone-700 mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-red-400">
            {items.map((item, i) => (
              <ScheduleItem key={i} item={item} />
            ))}
          </div>
        </React.Fragment>
      ))}
    </Item>
  );
};

Page.staticTitle = "Rozpis";
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_MEMBER];

export default Page;
