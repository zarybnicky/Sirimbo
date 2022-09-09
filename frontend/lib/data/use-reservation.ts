import { $, InputType, GraphQLTypes, NabidkaItemsOrderBy, Selector } from 'lib/zeus';

export const ReservationItemPartial = Selector('NabidkaItem')({
  niId: true,
  niLock: true,
  niPartner: true,
  niPocetHod: true,
  paryByNiPartner: {
    userByPIdPartner: {
      uJmeno: true,
      uId: true,
      uPrijmeni: true,
    },
    pId: true,
  },
});

export const ReservationPartial = Selector('Nabidka')({
  nOd: true,
  nDo: true,
  nId: true,
  nPocetHod: true,
  nMaxPocetHod: true,
  nLock: true,
  nTrener: true,
  nVisible: true,
  userByNTrener: {
    uJmeno: true,
    uId: true,
    uPrijmeni: true,
  },
  nabidkaItemsByNiIdRodic: [
    { orderBy: [NabidkaItemsOrderBy.NI_POCET_HOD_DESC] },
    { nodes: ReservationItemPartial },
  ],
});

export type ReservationItem = InputType<GraphQLTypes["NabidkaItem"], typeof ReservationItemPartial>;
export type Reservation = InputType<GraphQLTypes["Nabidka"], typeof ReservationPartial>;

export const ReservationRangeQuery = Selector('Query')({
  reservationsForRange: [
    { endDate: $('endDate', 'Date!'), startDate: $('startDate', 'Date!') },
    { nodes: ReservationPartial },
  ],
});
