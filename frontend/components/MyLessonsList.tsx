import * as React from 'react';
import { Box, IconButton, Typography } from '@mui/material';
import { useMyLessonsQuery, LessonFragment } from 'lib/graphql';
import { useAuth } from 'lib/data/use-auth';
import format from 'date-fns/format';
import Timeline from '@mui/lab/Timeline';
import TimelineItem from '@mui/lab/TimelineItem';
import TimelineSeparator from '@mui/lab/TimelineSeparator';
import TimelineConnector from '@mui/lab/TimelineConnector';
import TimelineContent from '@mui/lab/TimelineContent';
import TimelineDot from '@mui/lab/TimelineDot';
import TimelineOppositeContent from '@mui/lab/TimelineOppositeContent';
import NavigateBeforeIcon from '@mui/icons-material/NavigateBefore';
import NavigateNextIcon from '@mui/icons-material/NavigateNext';
import lastDayOfWeek from 'date-fns/lastDayOfWeek/index';
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
    const lessonsPerDay: { [day: string]: LessonFragment[] } = {};
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
    <Box display='flex' alignItems='center' justifyContent="right">
      <IconButton onClick={setPrevWeek}><NavigateBeforeIcon /></IconButton>
      <Typography color="textSecondary" component="span" align="right">
        {format(startDate, 'd. M. y')} - {format(lastDayOfWeek(startDate), 'd. M. y')}
      </Typography>
      <IconButton onClick={setNextWeek}><NavigateNextIcon /></IconButton>
    </Box>
    {Object.entries(lessonsPerDay).map(([key, lessons]) => <React.Fragment key={key}>
      <Typography variant="h6" align="center">
        {key}
      </Typography>
      <Timeline sx={{ flex: 1 }}>
        {lessons.map((lesson) => (
          <TimelineItem key={lesson.riId} sx={{ minHeight: '45px' }}>
            <TimelineOppositeContent color="text.secondary">
              {lesson.riOd.substring(0, 5)}&#8209;{lesson.riDo.substring(0, 5)}
            </TimelineOppositeContent>
            <TimelineSeparator>
              <TimelineDot />
              <TimelineConnector />
            </TimelineSeparator>
            <TimelineContent>
              {lesson.rozpiByRiIdRodic?.userByRTrener?.uId === user?.uId ? <>
                {lesson.paryByRiPartner?.userByPIdPartner?.uJmeno}{' '}
                {lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}
                {lesson.paryByRiPartner?.userByPIdPartnerka ? <>
                  {' - '}
                  {lesson.paryByRiPartner?.userByPIdPartnerka?.uJmeno}{' '}
                  {lesson.paryByRiPartner?.userByPIdPartnerka?.uPrijmeni}
                </> : ''}
              </> : <>
                <Typography>
                  {lesson.rozpiByRiIdRodic?.userByRTrener?.uJmeno}{' '}
                  {lesson.rozpiByRiIdRodic?.userByRTrener?.uPrijmeni}
                </Typography>
              </>}
            </TimelineContent>
          </TimelineItem>
        ))}
      </Timeline>
    </React.Fragment>)}
  </>;
};
