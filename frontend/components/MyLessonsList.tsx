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
    data?.myLessons?.nodes?.forEach((lesson) => {
      const date = lesson.rozpiByRiIdRodic?.rDatum;
      const place = lesson.rozpiByRiIdRodic?.rKde;
      const key = date ? `${place} ${formatWeekDay(new Date(date))}` : place ?? '';
      lessonsPerDay[key] = lessonsPerDay[key] || [];
      lessonsPerDay[key]!.push(lesson);
    });
    return lessonsPerDay;
  }, [data]);

  return (
    <div className="flex flex-col">
      <WeekPicker title="Moje lekce" startDate={startDate} onChange={setStartDate} />

      {!isLoading && !data?.myLessons?.nodes?.length && (
        <div className="text-stone-600">Žádné lekce tento týden</div>
      )}

      {Object.entries(lessonsPerDay).map(([key, lessons]) => (
        <Card key={key} className="grid w-72 rounded-lg border-stone-200 border">
          <h6 >
            {key.split(' ').map((x, i) => (
              <div key={x} className={i ? 'font-bold mb-1' : 'text-sm text-stone-500'}>{x}</div>
            ))}
          </h6>
          {lessons.map((lesson, i) => (
            <LessonButton
              key={i}
              lesson={lesson}
              schedule={lesson.rozpiByRiIdRodic!}
              showTrainer={lesson.rozpiByRiIdRodic?.rTrener !== user?.id}
            />
          ))}
        </Card>
      ))}
    </div>
  );
};
