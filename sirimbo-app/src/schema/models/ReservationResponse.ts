/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Reservation } from './Reservation';
import type { ReservationItemResponse } from './ReservationItemResponse';
import type { User } from './User';

export type ReservationResponse = {
    trainer: User;
    reservation: Reservation;
    items: Array<ReservationItemResponse>;
}
