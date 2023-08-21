import { AnnouncementListDocument } from '@app/graphql/Announcement';
import { PaymentCategoryListDocument, PaymentItemListDocument } from '@app/graphql/Payment';
import { CohortColorBoxes } from '@app/ui/CohortColorBox';
import { fullDateFormatter } from '@app/ui/format-date';
import { makeAdminList } from '@app/ui/generic/AdminEntityList';
import React from 'react';
import { AdminEntity } from '@app/ui/generic/AdminEntityList';

const PaymentItem: AdminEntity = {
  name: (n: number) => (n === 1 ? 'platba' : n > 1 && n < 5 ? 'platby' : 'plateb'),
  listRoute: '/platby/items',
  addRoute: '/platby/items/add',
  editRoute: (id) => `/platby/items/${id}`,
};

const PaymentCategory: AdminEntity = {
  name: (n: number) =>
    n === 1 ? 'kategorie' : n > 1 && n < 5 ? 'kategorie' : 'kategorií',
  listRoute: '/platby/structure/category',
  addRoute: '/platby/structure/category/add',
  editRoute: (id) => `/platby/structure/category/${id}`,
};

const Announcement: AdminEntity = {
  name: (n: number) =>
    n === 1 ? 'příspěvek' : n > 1 && n < 5 ? 'příspěvky' : 'příspěvků',
  listRoute: '/nastenka',
  addRoute: '/nastenka/add',
  editRoute: (id) => `/nastenka/${id}`,
};

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
