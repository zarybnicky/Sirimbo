import * as React from 'react';
import { useMyLessonsQuery, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { LessonButton } from './LessonButton';
import { Card } from './Card';
import { formatWeekDay } from 'lib/format-date';
import { WeekPicker, mondayToWeekRange, getCurrentMonday } from './WeekPicker';

export const MyLessonsList: React.FC = () => {
  const { user } = useAuth();
  const [startDate, setStartDate] = React.useState(getCurrentMonday);

  const { data, isLoading } = useMyLessonsQuery(mondayToWeekRange(startDate));

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

  return <div className="flex flex-col">
    <WeekPicker title="Moje lekce" startDate={startDate} onChange={setStartDate} />

    {!isLoading && !data?.myLessons?.nodes?.length && (
      <div className="text-stone-600">
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
  </div >;
};
