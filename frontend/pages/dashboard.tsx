import * as React from 'react';
import { MyAnnouncements } from 'components/MyAnnouncements';
import { MyLessonsList } from 'components/MyLessonsList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';

export default function DashboardPage() {
  return <Item className="col-feature">
    <div className="grid lg:grid-cols-2">
      <MyLessonsList />
      <MyAnnouncements />
    </div>
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
