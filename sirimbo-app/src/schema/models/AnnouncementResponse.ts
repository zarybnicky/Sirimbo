/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Announcement } from './Announcement';
import type { AnnouncementGroup } from './AnnouncementGroup';
import type { Key } from './Key';
import type { User } from './User';

export type AnnouncementResponse = {
    groups: Array<AnnouncementGroup>;
    announcement: Announcement;
    user: User;
    announcementId: Key;
}
