import * as React from 'react';
import { MyLessonsDocument, ScheduleItemFragment } from 'lib/graphql/Schedule';
import { useAuth } from 'lib/data/use-auth';
import { LessonButton } from './LessonButton';
import { Card } from './Card';
import { formatWeekDay } from 'lib/format-date';
import { WeekPicker, mondayToWeekRange, getCurrentMonday } from './WeekPicker';
import { CohortDocument } from 'lib/graphql/Cohorts';
import { RichTextView } from './RichTextView';
import { Schedule } from 'lib/entities';
import { useQuery } from 'urql';

export const MyLessonsList: React.FC = () => {
  const { user } = useAuth();
  const [startDate, setStartDate] = React.useState(getCurrentMonday);
  const [{ data: cohortData }] = useQuery({
    query: CohortDocument,
    variables: { id: user?.uSkupina! },
    pause: !user?.uSkupina,
  });
  const cohort = cohortData?.skupiny;

  const [{ data, fetching }] = useQuery({query: MyLessonsDocument, variables: mondayToWeekRange(startDate)})

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

      {!fetching && !data?.myLessons?.nodes?.length && (
        <div className="text-stone-600">Žádné lekce tento týden</div>
      )}

      <div className="flex flex-wrap flex-col gap-x-2">
        {Object.entries(lessonsPerDay).map(([key, lessons]) => (
          <LessonList
            key={key}
            location={key.split(' ')[0]!}
            day={key.split(' ')[1]!}
            lessons={lessons}
          />
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

type LessonListProps = {
  day: string;
  location: string;
  lessons: ScheduleItemFragment[];
};
function LessonList({ day, location, lessons }: LessonListProps) {
  const { user } = useAuth();
  const menu = Schedule.useMenu(lessons[0]?.rozpiByRiIdRodic || undefined);
  const isSameTrainer = lessons.every(
    (x, _, arr) => x.rozpiByRiIdRodic?.rTrener === arr[0]?.rozpiByRiIdRodic?.rTrener,
  );
  return (
    <Card
      className="grid w-72 rounded-lg border-stone-200 border"
      menu={isSameTrainer ? menu : []}
    >
      <h6>
        <div className="text-sm text-stone-500">{location}</div>
        <div className="font-bold mb-1">{day}</div>
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
  );
}
