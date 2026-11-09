import React from 'react';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { fullDateFormatter } from '@/ui/format';
import { getWeek } from 'date-fns/getWeek';
import { add, endOf, subtract } from 'date-arithmetic';
import { typographyCls } from './style';

type WeekPickerProps = {
  title: React.ReactNode;
  startDate: Date;
  onChange: React.Dispatch<React.SetStateAction<Date>>;
};

export function WeekPicker({ title, startDate, onChange }: WeekPickerProps) {
  const setPrevWeek = React.useCallback(() => {
    onChange((x) => subtract(x, 7, 'day'));
  }, [onChange]);

  const setNextWeek = React.useCallback(() => {
    onChange((x) => add(x, 7, 'day'));
  }, [onChange]);

  return (
    <div>
      <div className="flex justify-between items-center">
        <h4 className={typographyCls({ variant: 'smallHeading', className: 'grow' })}>
          {title}
        </h4>
        <div className="flex flex-nowrap">
          <button type="button" className="px-1.5" onClick={setPrevWeek}>
            <ChevronLeft />
          </button>
          <button type="button" className="px-1.5" onClick={setNextWeek}>
            <ChevronRight />
          </button>
        </div>
      </div>

      <div className="mb-2 flex items-center text-neutral-11 text-sm">
        {fullDateFormatter.formatRange(startDate, endOf(startDate, 'week', 1))}
        {` (${getWeek(startDate, { weekStartsOn: 1 })}. týden)`}
      </div>
    </div>
  );
}
