/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Day } from './Day';
import type { Key } from './Key';
import type { UTCTime } from './UTCTime';

export type User = {
    userPaymentGroup: Key;
    userOrientationNumber: string;
    userSystem: string;
    userBan: string;
    userUserGroup: Key;
    userConscriptionNumber: string;
    userDistrict: string;
    userSurname: string;
    userConfirmed: string;
    userNotes: string;
    userLogin: string;
    userTeacher: string;
    userStreet: string;
    userBirthDate: Day;
    userPassword: string;
    userCity: string;
    userGender: string;
    userName: string;
    userUpdatedAt: UTCTime;
    userPostalCode: string;
    userCreatedAt: UTCTime;
    userLevel: number;
    userGdprSignedAt?: UTCTime;
    userLock: string;
    userEmail: string;
    userPhone: string;
    userNationality: string;
    userMemberUntil?: UTCTime;
    userMemberSince?: UTCTime;
    userDancer: string;
}
