import { AdminEntity } from '@app/ui/generic/AdminEntityList';

interface AppAdminEntity extends AdminEntity {
  listRoute: string;
  addRoute: string;
  editRoute: (id: string) => string;
}

export const Article: AppAdminEntity = {
  name: (n) => (n === 1 ? 'článek' : n > 1 && n < 5 ? 'články' : 'článků'),
  listRoute: '/admin/aktuality',
  addRoute: '/admin/aktuality/add',
  editRoute: (id) => `/admin/aktuality/${id}`,
};

export const Couple: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'pár' : n > 1 && n < 5 ? 'páry' : 'párů'),
  listRoute: '/admin/pary',
  addRoute: '/admin/pary',
  editRoute: (id) => `/admin/pary/${id}`,
};

export const User: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'uživatel' : n > 1 && n < 5 ? 'uživatelé' : 'uživatelů',
  listRoute: '/admin/users',
  addRoute: '/admin/users/add',
  editRoute: (id) => `/admin/users/${id}`,
};

export const PaymentItem: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'platba' : n > 1 && n < 5 ? 'platby' : 'plateb'),
  listRoute: '/admin/platby/items',
  addRoute: '/admin/platby/items/add',
  editRoute: (id) => `/admin/platby/items/${id}`,
};

export const PaymentCategory: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'kategorie' : n > 1 && n < 5 ? 'kategorie' : 'kategorií',
  listRoute: '/admin/platby/structure/category',
  addRoute: '/admin/platby/structure/category/add',
  editRoute: (id) => `/admin/platby/structure/category/${id}`,
};

export const PaymentGroup: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'skupina' : n > 1 && n < 5 ? 'skupiny' : 'skupin'),
  listRoute: '/admin/platby/structure/group',
  addRoute: '/admin/platby/structure/group/add',
  editRoute: (id) => `/admin/platby/structure/group/${id}`,
};

export const Announcement: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'příspěvek' : n > 1 && n < 5 ? 'příspěvky' : 'příspěvků',
  listRoute: '/admin/nastenka',
  addRoute: '/admin/nastenka/add',
  editRoute: (id) => `/admin/nastenka/${id}`,
};

export const Schedule: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'rozpis' : n > 1 && n < 5 ? 'rozpisy' : 'rozpisů'),
  listRoute: '/admin/rozpis',
  addRoute: '/admin/rozpis/add',
  editRoute: (id) => `/admin/rozpis/${id}`,
};

export const Reservation: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'nabídka' : n > 1 && n < 5 ? 'nabídky' : 'nabídek'),
  listRoute: '/admin/nabidka',
  addRoute: '/admin/nabidka/add',
  editRoute: (id) => `/admin/nabidka/${id}`,
};

export const Event: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'akce' : n > 1 && n < 5 ? 'akce' : 'akcí'),
  listRoute: '/admin/akce',
  addRoute: '/admin/akce/add',
  editRoute: (id) => `/admin/akce/${id}`,
};

export const Cohort: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'skupina' : n > 1 && n < 5 ? 'skupiny' : 'skupin'),
  listRoute: '/admin/skupiny',
  addRoute: '/admin/skupiny/add',
  editRoute: (id) => `/admin/skupiny/${id}`,
};

export const CohortGroup: AppAdminEntity = {
  name: (n: number) =>
    n === 1
      ? 'tréninkový program'
      : n > 1 && n < 5
      ? 'tréninkové programy'
      : 'tréninkových programů',
  listRoute: '/admin/cohort-group',
  addRoute: '/admin/cohort-group/add',
  editRoute: (id) => `/admin/cohort-group/${id}`,
};
