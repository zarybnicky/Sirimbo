import { AdminEntity } from '@app/ui/generic/AdminEntityList';

interface AppAdminEntity extends AdminEntity {
  listRoute: string;
  addRoute: string;
  editRoute: (id: string) => string;
}

export const Article: AppAdminEntity = {
  name: (n) => (n === 1 ? 'článek' : n > 1 && n < 5 ? 'články' : 'článků'),
  listRoute: '/aktuality',
  addRoute: '/aktuality/add',
  editRoute: (id) => `/aktuality/${id}`,
};

export const Couple: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'pár' : n > 1 && n < 5 ? 'páry' : 'párů'),
  listRoute: '/pary',
  addRoute: '/pary',
  editRoute: (id) => `/pary/${id}`,
};

export const PaymentItem: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'platba' : n > 1 && n < 5 ? 'platby' : 'plateb'),
  listRoute: '/platby/items',
  addRoute: '/platby/items/add',
  editRoute: (id) => `/platby/items/${id}`,
};

export const PaymentCategory: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'kategorie' : n > 1 && n < 5 ? 'kategorie' : 'kategorií',
  listRoute: '/platby/structure/category',
  addRoute: '/platby/structure/category/add',
  editRoute: (id) => `/platby/structure/category/${id}`,
};

export const Announcement: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'příspěvek' : n > 1 && n < 5 ? 'příspěvky' : 'příspěvků',
  listRoute: '/nastenka',
  addRoute: '/nastenka/add',
  editRoute: (id) => `/nastenka/${id}`,
};
