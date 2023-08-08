import { CoupleFragment } from '@app/graphql/Couple';

export const formatCoupleName = ({ man, woman }: CoupleFragment) => {
  return woman
    ? `${man?.lastName} - ${woman.lastName}`
    : (['.', ',', '', undefined].includes(man?.lastName)
        ? man?.firstName
        : ['.', ',', '', undefined].includes(man?.firstName)
        ? man?.lastName
        : `${man?.firstName} ${man?.lastName}`) ?? '';
};
