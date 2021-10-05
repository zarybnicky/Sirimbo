/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Day } from './Day';
import type { Key } from './Key';
import type { UTCTime } from './UTCTime';

export type Reservation = {
    reservationTrainer: Key;
    reservationMaximumPerPair: number;
    reservationNumberLessons: number;
    reservationTo: Day;
    reservationFrom: Day;
    reservationVisible: string;
    reservationUpdatedAt: UTCTime;
    reservationLock: string;
}
