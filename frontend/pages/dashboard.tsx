import * as React from 'react';
import { MyAnnouncements } from 'components/MyAnnouncements';
import { MyLessonsList } from 'components/MyLessonsList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { TenantInformation } from 'components/TenantInformation';

export default function DashboardPage() {
  return <Item className="col-full">
    <div className="grid lg:grid-cols-3">
      <MyLessonsList />
      <MyAnnouncements />
      <TenantInformation />
    </div>
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
