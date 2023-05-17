import { FixCouplesButton } from 'components/FixCouplesButton';
import { makeAdminList } from 'components/generic/AdminEntityList';
import { Article, Couple, Reservation } from './entities';
import { fullDateFormatter } from './format-date';
import { formatCoupleName } from './format-name';
import { ArticlesDocument } from './graphql/Articles';
import { CoupleListDocument } from './graphql/Couple';
import { ReservationListDocument } from './graphql/Reservation';

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

export const CoupleList = makeAdminList(
  Couple,
  CoupleListDocument,
)(x => x.activeCouples?.nodes)((x) => ({
  id: x.id,
  title: formatCoupleName(x),
}))({
  headerExtra: <FixCouplesButton />,
});
