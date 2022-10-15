import { ReservationFragment, ReservationItemFragment, ScheduleFragment, ScheduleItemFragment, UserPartialFragment } from 'lib/graphql';
import { useAuth } from './use-auth';

export enum PermissionLevel {
  P_NONE = 1,
  P_VIEW = 2,
  P_MEMBER = 4,
  P_OWNED = 8,
  P_ADMIN = 16,
}

export enum PermissionKey {
  peAkce,
  peAktuality,
  peDokumenty,
  peGalerie,
  peNabidka,
  peNastenka,
  peNovinky,
  pePary,
  pePlatby,
  pePermissions,
  peRozpis,
  peSkupiny,
  peUsers,
  peMain,
};

type PermissionMap = { [key in keyof typeof PermissionKey]: PermissionLevel };

export const defaultPermissions: PermissionMap = {
  peAkce: 1,
  peAktuality: 1,
  peDokumenty: 1,
  peGalerie: 1,
  peNabidka: 1,
  peNastenka: 1,
  peNovinky: 1,
  pePary: 1,
  pePlatby: 1,
  pePermissions: 1,
  peRozpis: 1,
  peSkupiny: 1,
  peUsers: 1,
  peMain: 2,
};

export const usePermissions = () => {
  const { user, couple } = useAuth();
  const perms = user?.permissionByUGroup || defaultPermissions;

  return {
    canEditSchedule(schedule: ScheduleFragment) {
      return (
        (perms.peRozpis >= PermissionLevel.P_OWNED && user?.uId == schedule.rTrener) ||
        perms.peRozpis >= PermissionLevel.P_ADMIN
      );
    },
    canEditReservation(reservation: ReservationFragment) {
      return (
        (perms.peNabidka >= PermissionLevel.P_OWNED && user?.uId == reservation.nTrener) ||
        perms.peNabidka >= PermissionLevel.P_ADMIN
      );
    },
    canSignUp(item: ScheduleFragment, lesson: ScheduleItemFragment) {
      return (
        perms.peRozpis >= PermissionLevel.P_MEMBER &&
        (!lesson.riPartner || lesson.riPartner === '0') && !item.rLock && !lesson.riLock
      );
    },
    canSignOut(item: ScheduleFragment, lesson: ScheduleItemFragment) {
      return (
        (lesson.riPartner && lesson.riPartner !== '0') && !item.rLock && !lesson.riLock && (
          (perms.peRozpis >= PermissionLevel.P_MEMBER && couple?.pId == lesson.riPartner) ||
          (perms.peRozpis >= PermissionLevel.P_OWNED && user?.uId == item.rTrener) ||
          perms.peRozpis >= PermissionLevel.P_ADMIN
        )
      );
    },
    canMakeReservation(item: ReservationFragment) {
      return (
        perms.peNabidka >= PermissionLevel.P_MEMBER && !item.nLock &&
        item.nPocetHod > item.nabidkaItemsByNiIdRodic.nodes.reduce((n, x) => n + x.niPocetHod, 0)
      );
    },
    canCancelReservation(item: ReservationFragment, lesson: ReservationItemFragment) {
      return !item.nLock && !lesson.niLock && (
        (perms.peNabidka >= PermissionLevel.P_MEMBER && couple?.pId == lesson.niPartner) ||
        (perms.peNabidka >= PermissionLevel.P_OWNED && user?.uId == item.nTrener) ||
        perms.peNabidka >= PermissionLevel.P_ADMIN
      );
    },
  };
};
