import * as React from 'react';
import { useMyLessonsQuery, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { lastDayOfWeek } from 'date-fns';
import { LessonButton } from './LessonButton';
import { Card } from './Card';
import { formatWeekDay, fullDateFormatter } from 'lib/format-date';
import getWeek from 'date-fns/getWeek';

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

  const { data, isLoading } = useMyLessonsQuery({
    startDate: startDate.toISOString().substring(0, 10),
    endDate: lastDayOfWeek(startDate).toISOString().substring(0, 10),
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

  return <div className="flex flex-col items-center">
    <h4 className="text-2xl tracking-wide">Moje lekce</h4>
    <div className="flex items-center">
      <button className="button button-icon text-stone-500" onClick={setPrevWeek}>
        <ChevronLeft />
      </button>
      <div className="text-stone-500">
        {fullDateFormatter.formatRange(startDate, lastDayOfWeek(startDate))}
      </div>
      <button className="button button-icon text-stone-500" onClick={setNextWeek}>
        <ChevronRight />
      </button>
    </div>
    <div className="text-lg">
      {getWeek(startDate, { weekStartsOn: 2 })}. týden
    </div>

    {!isLoading && !data?.myLessons?.nodes?.length && (
      <div className="text-stone-600 p-4 text-center">
        Žádné lekce tento týden
      </div>
    )}

    {Object.entries(lessonsPerDay).map(([key, lessons]) => <React.Fragment key={key}>
      <h6 className="text-lg font-bold mb-1 mt-4">{key}</h6>

      <Card className="grid w-72 rounded-lg border-stone-200 border">
        {lessons.map((lesson, i) => (
          <LessonButton
            showTrainer={lesson.rozpiByRiIdRodic?.rTrener !== user?.id}
            key={i} schedule={lesson.rozpiByRiIdRodic!} lesson={lesson}
          />
        ))}
      </Card>
    </React.Fragment>)}
  </div>;
};
