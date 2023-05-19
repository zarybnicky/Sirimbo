import { FixCouplesButton } from 'components/FixCouplesButton';
import { makeAdminList } from 'components/generic/AdminEntityList';
import { Article, Cohort, CohortGroup, Couple, Event, Reservation, Schedule } from './entities';
import { fullDateFormatter } from './format-date';
import { formatCoupleName } from './format-name';
import { ArticlesDocument } from './graphql/Articles';
import { CohortGroupListDocument } from './graphql/CohortGroup';
import { CohortListDocument } from './graphql/Cohorts';
import { CoupleListDocument } from './graphql/Couple';
import { EventListDocument } from './graphql/Event';
import { ReservationListDocument } from './graphql/Reservation';
import { ScheduleListDocument } from './graphql/Schedule';

export const ArticleList = makeAdminList(
  Article,
  ArticlesDocument,
)((x) => x.aktualities?.nodes)((x) => ({
  id: x.id,
  title: x.atJmeno,
  subtitle: x.atTimestampAdd ? fullDateFormatter.format(new Date(x.atTimestampAdd)) : '',
}))({
  indexedFields: ['id', 'title']
});

export const ReservationList = makeAdminList(
  Reservation,
  ReservationListDocument,
)((x) => x.nabidkas?.nodes)((x) => ({
  id: x.id,
  title: x.userByNTrener?.fullName || '',
  subtitle: fullDateFormatter.formatRange(new Date(x.nOd), new Date(x.nDo)),
}))({
  indexedFields: ['id', 'title', 'subtitle'],
});

export const ScheduleList = makeAdminList(
  Schedule,
  ScheduleListDocument,
)((x) => x.rozpis?.nodes)((x) => ({
  id: x.id,
  title: x.userByRTrener?.fullName || '',
  subtitle: fullDateFormatter.format(new Date(x.rDatum)),
}))({
  indexedFields: ['id', 'title', 'subtitle'],
});

export const CoupleList = makeAdminList(
  Couple,
  CoupleListDocument,
)(x => x.activeCouples?.nodes)((x) => ({
  id: x.id,
  title: formatCoupleName(x),
}))({
  headerExtra: <FixCouplesButton />,
});

export const EventList = makeAdminList(
  Event,
  EventListDocument,
)(x => x.events?.nodes)(x => ({
  id: x.id,
  title: x.name,
  subtitle: fullDateFormatter.formatRange(new Date(x.since), new Date(x.until)),
}))({});

export const CohortList = makeAdminList(
  Cohort,
  CohortListDocument,
)(x => x.skupinies?.nodes)(x => ({
  id: x.id,
  title: x.sName,
  subtitle: [!x.sVisible && 'Skrytá', x.sLocation].filter(Boolean).join(', '),
  children: (
    <div
      className="absolute rounded-l-lg w-4 shadow-sm top-0 bottom-0 left-0"
      style={{ backgroundColor: x.sColorRgb }}
    />
  ),
}))({})

export const CohortGroupList = makeAdminList(
  CohortGroup,
  CohortGroupListDocument,
)(x => x.cohortGroups?.nodes)((x) => ({
  id: x.id,
  title: x.name,
  subtitle: x.isPublic ? '' : 'Skrytý',
}))({});
