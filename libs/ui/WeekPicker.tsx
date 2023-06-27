import React from 'react';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { fullDateFormatter } from '@app/ui/format-date';
import getWeek from 'date-fns/getWeek';
import { add, endOf, subtract } from 'date-arithmetic';

type WeekPickerProps = {
  title: React.ReactNode;
  startDate: Date;
  onChange: React.Dispatch<React.SetStateAction<Date>>;
};

export const WeekPicker = ({ title, startDate, onChange }: WeekPickerProps) => {
  const setPrevWeek = React.useCallback(() => {
    onChange((startDate) => subtract(startDate, 7, 'day'));
  }, [onChange]);

  const setNextWeek = React.useCallback(() => {
    onChange((startDate) => add(startDate, 7, 'day'));
  }, [onChange]);

  return (
    <div>
      <h4 className="mb-2 text-2xl tracking-wide">
        {title} ({getWeek(startDate, { weekStartsOn: 1 })}. týden)
      </h4>

      <div className="mb-2 flex items-center">
        <button className="button button-icon text-neutral-11" onClick={setPrevWeek}>
          <ChevronLeft />
        </button>
        <div className="text-neutral-11">
          {fullDateFormatter.formatRange(startDate, endOf(startDate, 'week', 1))}
        </div>
        <button className="button button-icon text-neutral-11" onClick={setNextWeek}>
          <ChevronRight />
        </button>
      </div>
    </div>
  );
};
