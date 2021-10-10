/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Announcement } from './Announcement';
import type { AnnouncementGroup } from './AnnouncementGroup';

export type AnnouncementResponse = {
    groups: Array<AnnouncementGroup>;
    announcement: Announcement;
}
