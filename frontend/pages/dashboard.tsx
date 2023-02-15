import * as React from 'react';
import { MyAnnouncements } from 'components/MyAnnouncements';
import { MyLessonsList } from 'components/MyLessonsList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { TenantInformation } from 'components/TenantInformation';
import { TabMenu } from 'components/TabMenu';

export default function DashboardPage() {
  const [variant, setVariant] = React.useState('myLessons');

  return <div className="col-feature lg:col-full mt-12 mb-8 mx-4">
    <div className="lg:hidden">
      <TabMenu selected={variant} onSelect={setVariant} options={[
        { id: 'myLessons', label: 'Moje lekce' },
        { id: 'myAnnouncements', label: 'Aktuality' },
        { id: 'importantInfo', label: 'Stálé informace' },
      ]} />
      <div className="pt-2">
        {variant === 'myLessons' ? <MyLessonsList /> :
          variant === 'myAnnouncements' ? <MyAnnouncements /> :
            <TenantInformation />}
      </div>
    </div>

    <div className="hidden lg:grid grid-cols-3 gap-4">
      <MyLessonsList />
      <MyAnnouncements />
      <TenantInformation />
    </div>
  </div>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
