import * as React from 'react';
import { MyLessonsDocument, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { LessonButton } from './LessonButton';
import { Card } from './Card';
import { formatWeekDay } from 'lib/format-date';
import { WeekPicker, mondayToWeekRange, getCurrentMonday } from './WeekPicker';
import { useGqlQuery } from 'lib/query';
import { CohortDocument } from 'lib/graphql/Cohorts';
import { RichTextView } from './RichTextView';
import { Schedule } from 'lib/entities';

export const MyLessonsList: React.FC = () => {
  const { user } = useAuth();
  const [startDate, setStartDate] = React.useState(getCurrentMonday);
  const { data: cohortData } = useGqlQuery(
    CohortDocument,
    { id: user?.uSkupina! },
    { enabled: !!user?.uSkupina },
  );
  const cohort = cohortData?.skupiny;

  const { data, isLoading } = useGqlQuery(
    MyLessonsDocument,
    mondayToWeekRange(startDate),
  );

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

      <div className="flex flex-wrap gap-x-2">
      {Object.entries(lessonsPerDay).map(([key, lessons]) => (
        <Card
          key={key}
          className="grid w-72 rounded-lg border-stone-200 border"
          menu={(lessons[0] && lessons.every((x, _, arr) => x.rozpiByRiIdRodic?.rTrener === arr[0]?.rozpiByRiIdRodic?.rTrener))
              ? Schedule.useMenu(lessons[0].rozpiByRiIdRodic!) : []}
        >
          <h6>
            {key.split(' ').map((x, i) => (
              <div key={x} className={i ? 'font-bold mb-1' : 'text-sm text-stone-500'}>
                {x}
              </div>
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

      {cohort && cohort.sVisible && (
        <>
          <h3 className="text-2xl tracking-wide mt-12 mb-4">Moje tréninková skupina</h3>
          <Card cohort={cohort}>
            <h3 className="text-2xl tracking-wide mb-4">{cohort.sName}</h3>
            <RichTextView value={cohort.sDescription} />
            <RichTextView value={cohort.internalInfo} />
          </Card>
        </>
      )}
    </div>
  );
};
