import { CohortColorBoxes } from '@app/ui/CohortColorBox';
import { makeAdminList } from '@app/ui/generic/AdminEntityList';
import {
  Announcement,
  Article,
  Cohort,
  CohortGroup,
  Couple,
  Event,
  PaymentCategory,
  PaymentGroup,
  PaymentItem,
  Reservation,
  Schedule,
} from './entities';
import { fullDateFormatter } from '@app/ui/format-date';
import { formatCoupleName } from '@app/ui/format-name';
import { AnnouncementListDocument } from '@app/graphql/Announcement';
import { ArticlesDocument } from '@app/graphql/Articles';
import { CohortGroupListDocument } from '@app/graphql/CohortGroup';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { CoupleListDocument } from '@app/graphql/Couple';
import { EventListDocument } from '@app/graphql/Event';
import { ReservationListDocument } from '@app/graphql/Reservation';
import { ScheduleListDocument } from '@app/graphql/Schedule';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { FixUnpairedCouplesDocument } from '@app/graphql/Couple';
import React from 'react';
import { PaymentCategoryListDocument, PaymentGroupListDocument, PaymentItemListDocument } from '@app/graphql/Payment';
import { Dialog, DialogContent, DialogTrigger } from './dialog';
import { NewCoupleForm } from './NewCoupleForm';
import { buttonCls } from './style/button';

export const ArticleList = makeAdminList(
  Article,
  ArticlesDocument,
)((x) => x.aktualities?.nodes)((x) => ({
  id: x.id,
  title: x.atJmeno,
  subtitle: x.atTimestampAdd ? fullDateFormatter.format(new Date(x.atTimestampAdd)) : '',
}))({
  indexedFields: ['id', 'title'],
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
)((x) => x.activeCouples?.nodes)((x) => ({
  id: x.id,
  title: formatCoupleName(x),
}))({
  disableAdd: true,
  Header() {
    const [open, setOpen] = React.useState(false);
    const fixMutation = useMutation(FixUnpairedCouplesDocument)[1];
    const fix = React.useCallback(async () => {
      const {data} = await fixMutation({});
      toast.info(`Opraveno ${data?.fixUnpairedCouples?.paries?.length || 0} záznamů`);
    }, [fixMutation]);

    return <>
      <Dialog open={open} onOpenChange={setOpen}>
        <DialogTrigger>
          <button className={buttonCls({ size: 'sm', variant: 'outline' })}>Přidat pár</button>
        </DialogTrigger>
        <DialogContent>
          <NewCoupleForm onSuccess={() => setOpen(false)} />
        </DialogContent>
      </Dialog>
      <button className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={fix}>
        Opravit nespárované páry
      </button>
    </>;
  },
});

export const EventList = makeAdminList(Event, EventListDocument)((x) => x.events?.nodes)(
  (x) => ({
    id: x.id,
    title: x.name,
    subtitle: fullDateFormatter.formatRange(new Date(x.since), new Date(x.until)),
  }),
)({});

export const PaymentGroupList = makeAdminList(
  PaymentGroup,
  PaymentGroupListDocument,
)((x) => x.platbyGroups?.nodes)((x) => ({
  id: x.id,
  title: x.pgName,
}))({});

export const PaymentCategoryList = makeAdminList(
  PaymentCategory,
  PaymentCategoryListDocument,
)((x) => x.platbyCategories?.nodes)((x) => ({
  id: x.id,
  title: x.pcName,
}))({});

export const PaymentItemList = makeAdminList(
  PaymentItem,
  PaymentItemListDocument,
)((x) => x.platbyItems?.nodes)((x) => ({
  id: x.id,
  title: [
    x.piAmount,
    x.platbyCategoryByPiIdCategory?.pcName,
    fullDateFormatter.format(new Date(x.piDate)),
    x.userByPiIdUser?.uJmeno,
    x.userByPiIdUser?.uPrijmeni
  ].join(' '),
}))({});

export const CohortList = makeAdminList(
  Cohort,
  CohortListDocument,
)((x) => x.skupinies?.nodes)((x) => ({
  id: x.id,
  title: x.sName,
  subtitle: [!x.sVisible && 'Skrytá', x.sLocation].filter(Boolean).join(', '),
  children: (
    <div
      className="absolute rounded-l-lg w-4 shadow-sm inset-y-0 left-0"
      style={{ backgroundColor: x.sColorRgb }}
    />
  ),
}))({});

export const CohortGroupList = makeAdminList(
  CohortGroup,
  CohortGroupListDocument,
)((x) => x.cohortGroups?.nodes)((x) => ({
  id: x.id,
  title: x.name,
  subtitle: x.isPublic ? '' : 'Skrytý',
}))({});

export const AnnouncementList = makeAdminList(
  Announcement,
  AnnouncementListDocument,
)((x) => x.upozornenis?.nodes)((x) => ({
  id: x.id,
  title: x.upNadpis,
  subtitle: (
    <div className="flex flex-wrap justify-between items-baseline gap-4">
      <div>
        {[
          x.userByUpKdo && `${x.userByUpKdo.uJmeno} ${x.userByUpKdo.uPrijmeni}`,
          fullDateFormatter.format(new Date(x.upTimestampAdd)),
        ]
          .filter(Boolean)
          .join(', ')}
      </div>
      <CohortColorBoxes
        items={x.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
          (x) => x.skupinyByUpsIdSkupina,
        )}
      />
    </div>
  ),
}))({
  pageSize: 100,
});