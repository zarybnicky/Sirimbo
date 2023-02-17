import React from "react";
import { ChevronLeft, ChevronRight } from 'react-feather';
import { fullDateFormatter } from 'lib/format-date';
import startOfWeek from 'date-fns/startOfWeek';
import lastDayOfWeek from "date-fns/lastDayOfWeek";
import endOfWeek from 'date-fns/endOfWeek';
import getWeek from 'date-fns/getWeek';
import endOfYear from "date-fns/endOfYear";

export const WeekPicker = ({ title, startDate, onChange }: {
  title: React.ReactNode;
  startDate: Date;
  onChange: React.Dispatch<React.SetStateAction<Date>>;
}) => {
  const setPrevWeek = React.useCallback(() => {
    onChange((startDate) => {
      const monday = new Date(startDate);
      monday.setDate(monday.getDate() - 7);
      return monday;
    });
  }, [])

  const setNextWeek = React.useCallback(() => {
    onChange((startDate) => {
      const monday = new Date(startDate);
      monday.setDate(monday.getDate() + 7);
      return monday;
    });
  }, [])

  return <div className="flex flex-col mb-4">
    <div className="flex items-center">
      <button className="button button-icon text-stone-500" onClick={setPrevWeek}>
        <ChevronLeft />
      </button>
      <div className="text-stone-500">
        {fullDateFormatter.formatRange(startDate, lastDayOfWeek(startDate, { weekStartsOn: 1 }))}
      </div>
      <button className="button button-icon text-stone-500" onClick={setNextWeek}>
        <ChevronRight />
      </button>
    </div>
    <h4 className="text-3xl tracking-wide">{title} ({getWeek(startDate, { weekStartsOn: 1 })}. týden)</h4>
  </div>;
};

export const getCurrentMonday = (): Date => {
  return startOfWeek(new Date(), { weekStartsOn: 1 });
};

export const mondayToWeekRange = (startDate: Date) => {
  return {
    startDate: startDate.toISOString().substring(0, 10),
    endDate: endOfWeek(startDate, { weekStartsOn: 1 }).toISOString().substring(0, 10),
  };
};

export const mondayToYearRange = (startDate: Date) => {
  return {
    startDate: startDate.toISOString().substring(0, 10),
    endDate: endOfYear(startDate).toISOString().substring(0, 10),
  };
};
