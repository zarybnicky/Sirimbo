import { $, InputType, GraphQLTypes, RozpisItemsOrderBy, Selector } from '../zeus';

export const ScheduleItemPartial = Selector('RozpisItem')({
  riOd: true,
  riDo: true,
  riId: true,
  riLock: true,
  riPartner: true,
  paryByRiPartner: {
    userByPIdPartner: {
      uJmeno: true,
      uId: true,
      uPrijmeni: true,
    },
    pId: true,
  },
});

export const SchedulePartial = Selector('Rozpi')({
  rDatum: true,
  rId: true,
  rKde: true,
  rLock: true,
  rTrener: true,
  rVisible: true,
  userByRTrener: {
    uJmeno: true,
    uId: true,
    uPrijmeni: true,
  },
  rozpisItemsByRiIdRodic: [
    { orderBy: [RozpisItemsOrderBy.RI_OD_ASC] },
    { nodes: ScheduleItemPartial },
  ],
});

export type ScheduleItem = InputType<GraphQLTypes["RozpisItem"], typeof ScheduleItemPartial>;
export type Schedule = InputType<GraphQLTypes["Rozpi"], typeof SchedulePartial>;

export const ScheduleRangeQuery = Selector('Query')({
  schedulesForRange: [
    { endDate: $`endDate`, startDate: $`startDate` },
    { nodes: SchedulePartial },
  ],
});
