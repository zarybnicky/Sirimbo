import * as React from 'react';
import { MyAnnouncements } from 'components/MyAnnouncements';
import { MyLessonsList } from 'components/MyLessonsList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { TenantInformation } from 'components/TenantInformation';
import { TabMenu } from 'components/TabMenu';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const [variant, setVariant] = React.useState('myLessons');

  return (
    <Item className="col-full-width p-2 bg-stone-100">
      <div className="xl:hidden">
        <TabMenu
          selected={variant}
          onSelect={setVariant}
          options={[
            { id: 'myLessons', label: 'Moje lekce' },
            { id: 'myAnnouncements', label: 'Aktuality' },
            { id: 'importantInfo', label: 'Stálé informace' },
          ]}
        />
        <div className="mt-4">
          {variant === 'myLessons' ? (
            <MyLessonsList />
          ) : variant === 'myAnnouncements' ? (
            <MyAnnouncements />
          ) : (
            <TenantInformation />
          )}
        </div>
      </div>

      <div className="hidden xl:grid grid-cols-3 gap-4">
        <MyLessonsList />
        <MyAnnouncements />
        <TenantInformation />
      </div>
    </Item>
  );
}

Page.staticTitle = "Nástěnka";
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
