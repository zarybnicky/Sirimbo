import { AnnouncementListDocument } from '@app/graphql/Announcement';
import { ArticlesDocument } from '@app/graphql/Articles';
import { PaymentCategoryListDocument, PaymentItemListDocument } from '@app/graphql/Payment';
import { CohortColorBoxes } from '@app/ui/CohortColorBox';
import { fullDateFormatter } from '@app/ui/format-date';
import { makeAdminList } from '@app/ui/generic/AdminEntityList';
import React from 'react';
import {
  Announcement,
  Article,
  PaymentCategory,
  PaymentItem,
} from './entities';

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
