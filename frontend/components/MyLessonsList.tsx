import * as React from 'react';
import { useMyLessonsQuery, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { format } from 'date-fns';
import { ChevronLeft, ChevronRight } from 'react-feather';
import { lastDayOfWeek } from 'date-fns';
import { cs } from 'date-fns/locale'
import { LessonButton } from './LessonButton';
import { Card } from './Card';

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
      let key = date ? format(new Date(date), 'EEEE d.', { locale: cs }) : '';
      key += key ? ` ${place}` : place;
      lessonsPerDay[key] = lessonsPerDay[key] || [];
      lessonsPerDay[key]!.push(lesson);
    });
    return lessonsPerDay;
  }, [data]);

  return <>
    <div className="flex items-center justify-center">
      <button className="button button-icon" onClick={setPrevWeek}>
        <ChevronLeft />
      </button>
      <span className="text-stone-700">
        {format(startDate, 'd. M. y')} - {format(lastDayOfWeek(startDate), 'd. M. y')}
      </span>
      <button className="button button-icon" onClick={setNextWeek}>
        <ChevronRight />
      </button>
    </div>
    {Object.entries(lessonsPerDay).map(([key, lessons]) => <React.Fragment key={key}>
      <h6 className="text-lg font-bold mb-2 text-center">{key}</h6>

      <Card className="grid mx-auto w-72 rounded-xl border-stone-200 border">
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
