import * as React from 'react';
import { useMyLessonsQuery, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { lastDayOfWeek } from 'date-fns';
import { LessonButton } from './LessonButton';
import { Card } from './Card';
import { formatFullDate, formatWeekDay } from 'lib/format-date';

export const MyLessonsList: React.FC = () => {
  const { user } = useAuth();
  const [startDate, setStartDate] = React.useState(() => {
    const today = new Date();
    const first = today.getDate() - today.getDay() + 1;
    const monday = new Date(today.setDate(first));
    return monday;
  });

  const setPrevWeek = React.useCallback(() => {
    setStartDate((startDate) => {
      const monday = new Date(startDate);
      monday.setDate(monday.getDate() - 7);
      return monday;
    });
  }, [])
  const setNextWeek = React.useCallback(() => {
    setStartDate((startDate) => {
      const monday = new Date(startDate);
      monday.setDate(monday.getDate() + 7);
      return monday;
    });
  }, [])

  const { data } = useMyLessonsQuery({
    startDate: '2020-01-01', // format(startDate, 'yyyy-MM-dd'),
    endDate: '2023-01-01'  // format(lastDayOfWeek(startDate), 'yyyy-MM-dd'),
  });

  const lessonsPerDay = React.useMemo(() => {
    const lessonsPerDay: { [day: string]: ScheduleItemFragment[] } = {};
    data?.myLessons?.nodes?.forEach(lesson => {
      const date = lesson.rozpiByRiIdRodic?.rDatum;
      const place = lesson.rozpiByRiIdRodic?.rKde;
      let key = date ? formatWeekDay(new Date(date)) : '';
      key += key ? ` ${place}` : place;
      lessonsPerDay[key] = lessonsPerDay[key] || [];
      lessonsPerDay[key]!.push(lesson);
    });
    return lessonsPerDay;
  }, [data]);

  return <>
    <div className="flex items-center">
      <button className="button button-icon" onClick={setPrevWeek}>
        <ChevronLeft />
      </button>
      <span className="text-stone-700">
        {formatFullDate(startDate)} - {formatFullDate(lastDayOfWeek(startDate))}
      </span>
      <button className="button button-icon" onClick={setNextWeek}>
        <ChevronRight />
      </button>
    </div>
    {Object.entries(lessonsPerDay).map(([key, lessons]) => <React.Fragment key={key}>
      <h6 className="text-lg font-bold mb-1 mt-4">{key}</h6>

      <Card className="grid w-72 rounded-xl border-stone-200 border">
        {lessons.map((lesson, i) => (
          <LessonButton
            showTrainer={lesson.rozpiByRiIdRodic?.rTrener !== user?.id}
            key={i} schedule={lesson.rozpiByRiIdRodic!} lesson={lesson}
          />
        ))}
      </Card>
    </React.Fragment>)}
  </>;
};
