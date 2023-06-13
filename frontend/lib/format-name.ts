import { CouplePartialFragment } from '@app/graphql/CurrentUser';

export const formatCoupleName = (couple: CouplePartialFragment | null) => {
  const man = couple?.userByPIdPartner;
  const woman = couple?.userByPIdPartnerka;
  return woman
    ? `${man?.uPrijmeni} - ${woman.uPrijmeni}`
    : (['.', ',', '', undefined].includes(man?.uPrijmeni)
        ? man?.uJmeno
        : ['.', ',', '', undefined].includes(man?.uJmeno)
        ? man?.uPrijmeni
        : `${man?.uJmeno} ${man?.uPrijmeni}`) ?? '';
};
