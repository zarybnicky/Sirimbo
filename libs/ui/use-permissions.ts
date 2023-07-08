import { ScheduleBasicFragment, ScheduleItemBasicFragment } from '@app/graphql/Schedule';
import {
  ReservationBasicFragment,
  ReservationItemBasicFragment,
} from '@app/graphql/Reservation';

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
}

export const defaultPermissions: {
  [key in keyof typeof PermissionKey]: PermissionLevel;
} = {
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

export const allowedPermissions: {
  [key in keyof typeof PermissionKey]: PermissionLevel[];
} = {
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

export const permissionMarks = [
  { value: 0, realValue: PermissionLevel.P_NONE, label: 'žádná' },
  { value: 1, realValue: PermissionLevel.P_VIEW, label: 'zobrazení' },
  { value: 2, realValue: PermissionLevel.P_MEMBER, label: 'člen' },
  { value: 3, realValue: PermissionLevel.P_OWNED, label: 'správa svých' },
  { value: 4, realValue: PermissionLevel.P_ADMIN, label: 'správa všech' },
];

export class PermissionChecker {
  constructor(
    public userId: string,
    public perms: { [key in keyof typeof PermissionKey]: number },
  ) {}

  public hasPermission(key: PermissionKey, level: PermissionLevel) {
    const perm = PermissionKey[key] as keyof typeof PermissionKey;
    return this.perms[perm] >= level;
  }

  public canEditArticle(article: { atKdo: string | null }) {
    return (
      (this.perms.peAktuality >= PermissionLevel.P_OWNED &&
        this.userId === article.atKdo) ||
      this.perms.peAktuality >= PermissionLevel.P_ADMIN
    );
  }

  public canEditSchedule(schedule: { rTrener: string }) {
    return (
      (this.perms.peRozpis >= PermissionLevel.P_OWNED &&
        this.userId === schedule.rTrener) ||
      this.perms.peRozpis >= PermissionLevel.P_ADMIN
    );
  }

  public canEditAnnouncement(item: { upKdo?: string | null }) {
    return (
      (this.perms.peNastenka >= PermissionLevel.P_OWNED && this.userId === item.upKdo) ||
      this.perms.peNastenka >= PermissionLevel.P_ADMIN
    );
  }

  public canEditCohort(_item: {}) {
    return this.perms.peSkupiny >= PermissionLevel.P_ADMIN;
  }

  public canEditEvent(_item: {}) {
    return this.perms.peAkce >= PermissionLevel.P_OWNED;
  }

  public canEditReservation(reservation: { nTrener: string }) {
    return (
      (this.perms.peNabidka >= PermissionLevel.P_OWNED &&
        this.userId === reservation.nTrener) ||
      this.perms.peNabidka >= PermissionLevel.P_ADMIN
    );
  }

  public canSignUp(
    item: { rDatum: string; rLock: boolean; rTrener: string },
    lesson: { riLock: boolean; riPartner: string | null },
  ) {
    return (
      this.perms.peRozpis >= PermissionLevel.P_MEMBER &&
      (!lesson.riPartner || lesson.riPartner === '0') &&
      !item.rLock &&
      !lesson.riLock &&
      +new Date(item.rDatum) >= +new Date()
    );
  }

  public canSignOut(item: ScheduleBasicFragment, lesson: ScheduleItemBasicFragment) {
    const man = lesson.paryByRiPartner?.userByPIdPartner;
    const woman = lesson.paryByRiPartner?.userByPIdPartner;
    const isMyLesson = this.userId === man?.id || this.userId === woman?.id;
    return (
      lesson.riPartner &&
      lesson.riPartner !== '0' &&
      !item.rLock &&
      !lesson.riLock &&
      +new Date(item.rDatum) >= +new Date() &&
      ((this.perms.peRozpis >= PermissionLevel.P_MEMBER && isMyLesson) ||
        (this.perms.peRozpis >= PermissionLevel.P_OWNED && this.userId == item.rTrener) ||
        this.perms.peRozpis >= PermissionLevel.P_ADMIN)
    );
  }

  public canMakeReservation(item: {
    nabidkaItemsByNiIdRodic: { nodes: { niPocetHod: number }[] };
    nTrener: string;
    nLock: boolean;
    nPocetHod: number;
  }) {
    return (
      this.perms.peNabidka >= PermissionLevel.P_MEMBER &&
      !item.nLock &&
      item.nPocetHod >
        item.nabidkaItemsByNiIdRodic.nodes.reduce((n, x) => n + x.niPocetHod, 0)
    );
  }

  public canCancelReservation(
    item: ReservationBasicFragment,
    lesson: ReservationItemBasicFragment,
  ) {
    const man = lesson.paryByNiPartner?.userByPIdPartner;
    const woman = lesson.paryByNiPartner?.userByPIdPartner;
    const isMyLesson = this.userId === man?.id || this.userId === woman?.id;
    return (
      !item.nLock &&
      !lesson.niLock &&
      ((this.perms.peNabidka >= PermissionLevel.P_MEMBER && isMyLesson) ||
        (this.perms.peNabidka >= PermissionLevel.P_OWNED &&
          this.userId == item.nTrener) ||
        this.perms.peNabidka >= PermissionLevel.P_ADMIN)
    );
  }
}
