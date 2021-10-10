/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Key } from './Key';
import type { UTCTime } from './UTCTime';

export type Announcement = {
    announcementText: string;
    announcementColors: number;
    announcementUpdatedAt: UTCTime;
    announcementTitle: string;
    announcementCreatedAt: UTCTime;
    announcementLock: string;
    announcementCreatedBy: Key;
}
