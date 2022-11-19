import * as React from 'react';
import { AnnouncementList } from '../components/AnnouncementList';
import { MyLessonsList } from 'components/MyLessonsList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';

export default function DashboardPage() {
  return <Item className="col-popout">
    <div className="grid lg:grid-cols-2">
      <div>
        <h4 className="text-lg font-bold">Moje tréninky</h4>
        <MyLessonsList />
      </div>
      <div>
        <h4 className="text-lg font-bold">Nástěnka</h4>
        <AnnouncementList />
      </div>
    </div>
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
