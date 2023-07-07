import { AdminEntity } from '@app/ui/generic/AdminEntityList';
import { LinkProps } from 'next/link'
type Route = LinkProps['href'];

interface AppAdminEntity extends AdminEntity {
  listRoute: Route;
  addRoute: Route;
  editRoute: (id: string) => Route;
}

export const Article: AppAdminEntity = {
  name: (n) => (n === 1 ? 'článek' : n > 1 && n < 5 ? 'články' : 'článků'),
  listRoute: { pathname: '/admin/aktuality' },
  addRoute: { pathname: '/admin/aktuality/add' },
  editRoute: (id) => ({ pathname: '/admin/aktuality/[id]', query: { id } }),
};

export const Couple: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'pár' : n > 1 && n < 5 ? 'páry' : 'párů'),
  listRoute: { pathname: '/admin/pary' },
  addRoute: { pathname: '/admin/pary/add' },
  editRoute: (id: string) => ({ pathname: '/admin/pary/[id]', query: { id } }),
};

export const User: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'uživatel' : n > 1 && n < 5 ? 'uživatelé' : 'uživatelů',
  listRoute: { pathname: '/admin/users' },
  addRoute: { pathname: '/admin/users/add' },
  editRoute: (id: string) => ({ pathname: '/admin/users/[id]', query: { id } }),
};

export const PaymentItem: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'platba' : n > 1 && n < 5 ? 'platby' : 'plateb'),
  listRoute: { pathname: '/admin/platby/items' },
  addRoute: { pathname: '/admin/platby/items/add' },
  editRoute: (id: string) => ({ pathname: '/admin/platby/items/[id]', query: { id } }),
};

export const PaymentCategory: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'kategorie' : n > 1 && n < 5 ? 'kategorie' : 'kategorií',
  listRoute: { pathname: '/admin/platby/structure/category' },
  addRoute: { pathname: '/admin/platby/structure/category/add' },
  editRoute: (id: string) => ({
    pathname: '/admin/platby/structure/category/[id]',
    query: { id },
  }),
};

export const PaymentGroup: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'skupina' : n > 1 && n < 5 ? 'skupiny' : 'skupin'),
  listRoute: { pathname: '/admin/platby/structure/group' },
  addRoute: { pathname: '/admin/platby/structure/group/add' },
  editRoute: (id: string) => ({
    pathname: '/admin/platby/structure/group/[id]',
    query: { id },
  }),
};

export const Announcement: AppAdminEntity = {
  name: (n: number) =>
    n === 1 ? 'příspěvek' : n > 1 && n < 5 ? 'příspěvky' : 'příspěvků',
  listRoute: { pathname: '/admin/nastenka' },
  addRoute: { pathname: '/admin/nastenka/add' },
  editRoute: (id: string) => ({ pathname: '/admin/nastenka/[id]', query: { id } }),
};

export const Schedule: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'rozpis' : n > 1 && n < 5 ? 'rozpisy' : 'rozpisů'),
  listRoute: { pathname: '/admin/rozpis' },
  addRoute: { pathname: '/admin/rozpis/add' },
  editRoute: (id: string) => ({ pathname: '/admin/rozpis/[id]', query: { id } }),
};

export const Reservation: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'nabídka' : n > 1 && n < 5 ? 'nabídky' : 'nabídek'),
  listRoute: { pathname: '/admin/nabidka' },
  addRoute: { pathname: '/admin/nabidka/add' },
  editRoute: (id: string) => ({ pathname: '/admin/nabidka/[id]', query: { id } }),
};

export const Event: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'akce' : n > 1 && n < 5 ? 'akce' : 'akcí'),
  listRoute: { pathname: '/admin/akce' },
  addRoute: { pathname: '/admin/akce/add' },
  editRoute: (id: string) => ({ pathname: '/admin/akce/[id]', query: { id } }),
};

export const Cohort: AppAdminEntity = {
  name: (n: number) => (n === 1 ? 'skupina' : n > 1 && n < 5 ? 'skupiny' : 'skupin'),
  listRoute: { pathname: '/admin/skupiny' },
  addRoute: { pathname: '/admin/skupiny/add' },
  editRoute: (id: string) => ({ pathname: '/admin/skupiny/[id]', query: { id } }),
};

export const CohortGroup: AppAdminEntity = {
  name: (n: number) =>
    n === 1
      ? 'tréninkový program'
      : n > 1 && n < 5
      ? 'tréninkové programy'
      : 'tréninkových programů',
  listRoute: { pathname: '/admin/cohort-group' },
  addRoute: { pathname: '/admin/cohort-group/add' },
  editRoute: (id: string) => ({ pathname: '/admin/cohort-group/[id]', query: { id } }),
};
