import * as React from 'react';
import { MyAnnouncements } from '@app/ui/MyAnnouncements';
import { MyLessonsList } from '@app/ui/MyLessonsList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { TabMenu } from '@app/ui/TabMenu';
import { StickyAnnouncements } from '@app/ui/StickyAnnouncements';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';
import { useQueryParam, withDefault, StringParam } from 'use-query-params';

const Page = () => {
  const [variant, setVariant] = useQueryParam('tab', withDefault(StringParam, 'myLessons'));

  return (
    <Layout permissions={[PermissionKey.peNastenka, PermissionLevel.P_VIEW]}>
      <NextSeo title="Nástěnka" />
      <div className="col-full-width p-4 lg:py-8">
        <div className="xl:hidden">
          <TabMenu
            selected={variant}
            onSelect={setVariant}
            options={[
              { id: 'myLessons', label: 'Moje lekce' },
              { id: 'myAnnouncements', label: 'Aktuality' },
              { id: 'stickyAnnouncements', label: 'Stálá nástěnka' },
            ]}
          />
          <div className="mt-4">
            {variant === 'myLessons' ? (
              <MyLessonsList />
            ) : variant === 'myAnnouncements' ? (
              <MyAnnouncements />
            ) : (
              <StickyAnnouncements />
            )}
          </div>
        </div>

        <div className="hidden xl:grid grid-cols-3 gap-4">
          <MyLessonsList />
          <MyAnnouncements />
          <StickyAnnouncements />
        </div>
      </div>
    </Layout>
  );
}

export default Page;
