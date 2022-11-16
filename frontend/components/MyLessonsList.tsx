import * as React from 'react';
import { useMyLessonsQuery, ScheduleItemFragment } from 'lib/graphql';
import { useAuth } from 'lib/data/use-auth';
import { format } from 'date-fns';
import NavigateBeforeIcon from '@mui/icons-material/NavigateBefore';
import NavigateNextIcon from '@mui/icons-material/NavigateNext';
import { lastDayOfWeek } from 'date-fns';
import { cs } from 'date-fns/locale'

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
        <NavigateBeforeIcon />
      </button>
      <span className="text-slate-700 text-right">
        {format(startDate, 'd. M. y')} - {format(lastDayOfWeek(startDate), 'd. M. y')}
      </span>
      <button className="button button-icon" onClick={setNextWeek}>
        <NavigateNextIcon />
      </button>
    </div>
    {Object.entries(lessonsPerDay).map(([key, lessons]) => <React.Fragment key={key}>
      <h6 className="text-center">{key}</h6>

      <div className="grid gap-2">
        {lessons.map((lesson, i) => (
          <div key={i} className="px-2 pt-1 rounded-md bg-slate-50">
            <div className="text-stone-700 leading-4 text-sm tabular-nums">
              {lesson.riOd.substring(0, 5)}&#8209;{lesson.riDo.substring(0, 5)}
            </div>
            <div className="leading-6">
              {lesson.rozpiByRiIdRodic?.userByRTrener?.uId === user?.uId ? <>
                {lesson.paryByRiPartner?.userByPIdPartner?.uJmeno}{' '}
                {lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}
                {lesson.paryByRiPartner?.userByPIdPartnerka ? <>
                  {' - '}
                  {lesson.paryByRiPartner?.userByPIdPartnerka?.uJmeno}{' '}
                  {lesson.paryByRiPartner?.userByPIdPartnerka?.uPrijmeni}
                </> : ''}
              </> : <>
                {lesson.rozpiByRiIdRodic?.userByRTrener?.uJmeno}{' '}
                {lesson.rozpiByRiIdRodic?.userByRTrener?.uPrijmeni}
              </>}
            </div>
          </div>
        ))}
      </div>
    </React.Fragment>)}
  </>;
};
