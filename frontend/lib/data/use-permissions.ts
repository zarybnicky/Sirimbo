import { ReservationFragment, ReservationItemFragment, ScheduleFragment, ScheduleItemFragment } from 'lib/graphql';
import { keysOf } from 'lib/keys-of';
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

const defaultPermissions: { [key in keyof typeof PermissionKey]: PermissionLevel } = {
  peAkce: 1,
  peAktuality: 2,
  peDokumenty: 1,
  peGalerie: 2,
  peNabidka: 1,
  peNastenka: 1,
  peNovinky: 1,
  pePary: 1,
  pePlatby: 1,
  pePermissions: 1,
  peRozpis: 1,
  peSkupiny: 2,
  peUsers: 1,
  peMain: 2,
};

export const allowedPermissions: { [key in keyof typeof PermissionKey]: PermissionLevel[] } = {
  peAkce: [1, 2, 4, 8, 16],
  peAktuality: [2, 8, 16],
  peDokumenty: [1, 4, 8, 16],
  peGalerie: [2, 8, 16],
  peNabidka: [1, 2, 4, 8, 16],
  peNastenka: [1, 2, 8, 16],
  peNovinky: [1, 2, 8, 16],
  pePary: [1, 2, 16],
  pePlatby: [1, 16],
  pePermissions: [1, 16],
  peRozpis: [1, 2, 4, 8, 16],
  peSkupiny: [1, 2, 16],
  peUsers: [1, 2, 8, 16],
  peMain: [2],
};

export const permissionLabels: { [key in keyof typeof PermissionKey]: string } = {
  peAkce: "Akce",
  peAktuality: "Aktuality",
  peDokumenty: "Dokumenty",
  peGalerie: "Galerie",
  peNabidka: "Nabídky",
  peNastenka: "Nástěnka",
  peNovinky: "Novinky",
  pePary: "Páry",
  pePlatby: "Platby",
  pePermissions: "Uživatelské role",
  peRozpis: "Rozpisy",
  peSkupiny: "Skupiny",
  peUsers: "Uživatelé",
  peMain: "Veřejná sekce",
};

export const realPermissionKeys = keysOf(PermissionKey).filter(key => (!~~key && key.toString() !== "0"));
export const realPermissionLevels = keysOf(PermissionLevel).filter(key => !(!~~key && key.toString() !== "0"));

export const permissionMarks = [
  { value: 0, realValue: PermissionLevel.P_NONE, label: 'žádná' },
  { value: 1, realValue: PermissionLevel.P_VIEW, label: 'zobrazení' },
  { value: 2, realValue: PermissionLevel.P_MEMBER, label: 'člen' },
  { value: 3, realValue: PermissionLevel.P_OWNED, label: 'správa svých' },
  { value: 4, realValue: PermissionLevel.P_ADMIN, label: 'správa všech' },
];

export const usePermissions = () => {
  const { user, couple } = useAuth();
  const perms = user?.permissionByUGroup || defaultPermissions;

  return {
    hasPermission(key: PermissionKey, level: PermissionLevel) {
      const perm = PermissionKey[key] as keyof typeof PermissionKey;
      return perms[perm] >= level;
    },
    canEditSchedule(schedule: ScheduleFragment) {
      return (
        (perms.peRozpis >= PermissionLevel.P_OWNED && user?.uId === schedule.rTrener) ||
        perms.peRozpis >= PermissionLevel.P_ADMIN
      );
    },
    canEditReservation(reservation: ReservationFragment) {
      return (
        (perms.peNabidka >= PermissionLevel.P_OWNED && user?.uId === reservation.nTrener) ||
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
