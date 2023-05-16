import * as React from 'react';
import { MyAnnouncements } from 'components/MyAnnouncements';
import { MyLessonsList } from 'components/MyLessonsList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
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
          ]}
        />
        <div className="mt-4">
          {variant === 'myLessons' ? (
            <MyLessonsList />
          ) : (
            <MyAnnouncements />
          )}
        </div>
      </div>

      <div className="hidden xl:grid grid-cols-2 gap-4">
        <MyLessonsList />
        <MyAnnouncements />
      </div>
    </Item>
  );
}

Page.staticTitle = "Nástěnka";
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
